import CodeMirror from 'codemirror';

// this file does not export anything

CodeMirror.registerHelper("syntaxCheck", "pollen", function(cm, line) {
  let state = cm.getStateAfter(line, true);
  return state.braceCount === 0 && state.blockBraceCount === 0;
});


/// get the name of the scope. Scopes are "inTopText", "inDatum", "inCmd"
/// "inBrace", "inBlockBrace"
CodeMirror.registerHelper("getScope", "pollen", function(cm, pos) {
  let token = cm.getTokenAt(pos);
  let state = token.state;
  return state.token.name;
});


function context(token, braceCount, blockBraceCount) {
  return {
    token: token,
    braceCount: braceCount,
    blockBraceCount: blockBraceCount,
  };
}

/// restore the
function restoreContext(state) {
  /// invariance check
  if (state.braceCount !== 0 || state.blockBraceCount !== 0) {
    throw new Error("pop context, but current context has braces not closed");
  }

  let {token, braceCount, blockBraceCount} = state.context.pop();
  state.token = token;
  state.braceCount = braceCount;
  state.blockBraceCount = blockBraceCount;
}

function saveContext(state, newToken) {
  let {token, braceCount, blockBraceCount} = state;
  let thisCtx = context(token, braceCount, blockBraceCount);
  state.context.push(thisCtx);
  state.token = newToken;
  state.braceCount = 0;
  state.blockBraceCount = 0;
}


/// BNF:
///
///        <char> ::= any character
///   <racket-id> ::= [^ \n(){}[]",'`;#|\\]+
///                 | '|' <char>+ '|'
///  <left-brace> ::= '{' | '|{'
/// <right-brace> ::= '}' | '}|'
///         <tag> ::= '◊' <racket-id>
///                 | '◊;'
///         <cmd> ::= <tag> <left-brace> <text> <right-brace>
///                 | <tag> '[' <datum> ']'
///                 | <tag> '[' <datum> ']' <left-brace> <text> <right-brace>
///       <datum> ::= <racket-expr>
///        <text> ::= <char> <text>
///                 | <cmd>
///
/// the important nonterminals are cmd, datum, text. In the implementation,
/// those nonterminals are named inDatum, inCmd, inBlockBrace, inBrace. State
/// transitions are controlled in state.token
function mode(config) {
  const cmdChar = config['command-char'] || '◊';
  // NOTE: racketId does not include cmdChar
  const racketId = `[^ \\n(){}\\[\\]",'\`;#|\\\\${cmdChar}]+`;
  const racketIdRegex = new RegExp(racketId);
  const nonScopeRegex = new RegExp(`[^${cmdChar}]`);

  function startState() {
    return {
      // context contains the rest of the state in previous context
      context: [],
      // the parser for the next token
      token: inTopText,
      // the brace { count of CURRENT tag (tag is opened by {)
      braceCount: 0,
      // the block brace |{ count of CURRENT tag (tag is opened by |{)
      blockBraceCount: 0,
      // tag stack from top level text
      tagStack: [],
      // in a block comment (comments start with { or |{)
      inComment: false
    };
  }

  function copyState(state) {
    let newState = {...state};
    newState.context = state.context.slice();
    newState.tagStack = state.tagStack.slice();
    return newState;
  }

  // it needs this method to decide whether to revert current state's
  // inComment to false from true
  function isInComments(state) {
    for (let tag of state.tagStack) {
      if (tag === ';') {
        return true;
      }
    }
    return false;
  }

  /// parse anything that's not in a cmd
  function inTopText(stream, state) {
    if (stream.eatSpace()) {
      return null;
    }

    stream.eatWhile(nonScopeRegex);
    return null;
  }

  /// inBrace keeps track of { and }. This method tries its best to
  /// produce token in space-separated words
  function inBrace(stream, state) {
    let nextChar = stream.next();
    if (nextChar === '{') {
      // this { has no tag in front of it, just track
      state.braceCount += 1;
      return 'open brace';
    }

    if (nextChar === '}') {
      if (state.braceCount > 0) {
        state.braceCount -= 1;
        if (state.braceCount === 0) {
          // this } closes a tag, so we need to pop context and tag
          restoreContext(state);
          let tag = state.tagStack.pop();
          if (tag === ';') {
            state.inComment = isInComments(state);
            return 'comment';
          }
        }
        return 'close brace';
      }
      return 'brace';
    }
    let eaten = nextChar === ' ' || stream.eatSpace();
    if (! eaten) {
      stream.eatWhile(/[^\s{}]/);
    }

    return null;
  }

  /// inBlockBrace keeps track of |{ and }|
  function inBlockBrace(stream, state) {
    if (stream.match(/\|\{/)) {
      state.blockBraceCount += 1;
      return 'open blockBrace';
    }
    if (stream.match(/\}\|/)) {
      if (state.blockBraceCount > 0) {
        state.blockBraceCount -= 1;
        if (state.blockBraceCount === 0) {
          // this }| closes a tag, pop context and tag
          restoreContext(state);
          let tag = state.tagStack.pop();
          if (tag === ';') {
            state.inComment = isInComments(state);
            return 'comment';
          }
        }
        return 'close blockBrace';
      }
      return 'blockBrace';
    }
    let eaten = stream.eatWhile(/[^\s{}]/);
    if (! eaten) {
      stream.next();
    }
    return null;
  }

  /// inCmd transition the state to other Nonterminals. i.e. it
  /// opereates on the boundries of components of a cmd.
  function inCmd(stream, state) {
    if (stream.peek() === cmdChar) {
      throw new Error("the caller of inCmd must eat cmdChar");
    }

    if (stream.eat(';')) {
      if (! stream.match(/\|?\{/, false)) {
        stream.skipToEnd();
        restoreContext(state);
      } else {
        state.tagStack.push(';');
        state.inComment = true;
      }
      return 'comment';
    }

    if (stream.match(racketIdRegex)) {
      let tag = stream.current().substring(1);
      state.tagStack.push(tag);
      return 'keyword';
    }

    let nextChar = stream.peek();
    if (nextChar === '[') {
      saveContext(state, inDatum);
      return null;
    }

    if (nextChar === '{') {
      state.token = inBrace;
      return null;
    }

    if (stream.match(/\|\{/, false)) {
      state.token = inBlockBrace;
      return null;
    }

    // not in Cmd anymore, restore previous context
    restoreContext(state);
    state.tagStack.pop();
    stream.next();

    return null;
  }

  function inDatum(stream, state) {
    let ch = stream.next();

    if (ch === '[') {
      return 'open bracket';
    }

    if (ch === "]") {
      // close this datum, pop context, keep the tag so previous
      // token can decide what to do
      restoreContext(state);
      return 'close bracket';
    }

    return null;
  }

  function token(stream, state) {
    // all states transit to <cmd> if next char is cmdChar
    if (stream.eat(cmdChar)) {
      saveContext(state, inCmd);
      let tokenStyle = inCmd(stream, state);
      return state.inComment ? 'comment' : tokenStyle;
    }

    let tokenStyle = state.token(stream, state);
    return state.inComment ? 'comment' : tokenStyle;
  }

  return {
    token,
    startState,
    copyState,
  };
}

CodeMirror.defineMode("pollen", mode);
CodeMirror.defineMIME("text/x-pollen", "pollen");
