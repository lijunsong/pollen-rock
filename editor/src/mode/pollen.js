import CodeMirror from 'codemirror';

// this file does not export anything

CodeMirror.registerHelper("syntaxCheck", "pollen", function(cm, line) {
  let state = cm.getStateAfter(line, true);
  return state.braceStack.length === 0;
});



const braces = {
  "{": "}",
  "|{": "}|",
  "}": "{",
  "}|": "|{",
  "[": "]",
  "]": "[",
};


function last(arr) {
  return arr[arr.length - 1] || null;
}

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
function mode(config) {
  const cmdChar = config['command-char'] || '◊';
  const racketId = `[^ \\n(){}\\[\\]",'\`;#|\\\\${cmdChar}]+`;
  const racketIdRegex = new RegExp(racketId);
  const commentTag = `${cmdChar};`;

  function startState() {
    return {
      // context contains the rest of the state in previous context
      context: [],
      // the parser for the next token
      token: inTopText,
      // the brace { count of CURRENT tag (tag is opened by {)
      braceCount: 0,
      // the block brace |{ count of CURRENT tag (tag is opened by |{)
      BlockBraceCount: 0,
      // tag stack from top level text
      tagStack: [],
    };
  }

  /// consume the tag if a tag is at the parse position. Return tag
  /// style or null
  function gotoCmd(stream, state) {
    if (stream.peak(cmdChar)) {
      state.token = inCmd;
      return inCmd(stream, state);
    }
    return null;
  }

  /// parse anything that's not in a cmd
  function inTopText(stream, state) {
    stream.next();
    return null;
  }

  /// inBrace keeps track of { (i.e. to count |{ as a simple brace,
  /// not a blockBrace)
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
        }
        return 'close brace';
      }
      return 'brace';
    }

    return null;
  }

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
        }
        return 'close blockBrace';
      }
      return 'blockBrace';
    }
    stream.next();
    return null;
  }

  /// inCmd transition the state to other Nonterminals. i.e. it
  /// opereates on the margin of components of a cmd.
  function inCmd(stream, state) {
    if (stream.match(racketIdRegex)) {
      let tag = stream.current();
      state.tagStack.push(tag);
      return 'keyword';
    }

    let nextToken = null;
    if (stream.eat('[')) {
      nextToken = inDatum;
    }

    if (stream.eat("{")) {
      nextToken = inBrace;
    }

    if (stream.match(/\|\{/)) {
      nextToken = inBlockBrace;
    }

    if (nextToken) {
      saveContext(nextToken);
    } else {
      stream.next();
    }

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
    let tokenStyle = gotoCmd();
    if (tokenStyle) {
      return tokenStyle;
    }
    tokenStyle = state.token(stream, state);
    return tokenStyle;
  }
}

CodeMirror.defineMode("pollen", mode);
CodeMirror.defineMIME("text/x-pollen", "pollen");
