import CodeMirror from 'codemirror';


CodeMirror.registerHelper("syntaxCheck", "pollen", function(cm, line) {
  return true;
  let state = cm.getStateAfter(line, true);
  return state.braceStack.length === 0;
});

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
  let cmdChar = config['command-char'] || '◊';
  let racketId = `[^ \\n(){}\\[\\]",'\`;#|\\\\${cmdChar}]+`;
  let racketIdRegex = new RegExp(racketId);
  let commentTag = `${cmdChar};`

  function leftRightBraceMatch(left, right) {
    if (left === '{' && right === '}') {
      return true;
    }

    if (left === '|{' && right === '}|') {
      return true;
    }

    return false;
  }

  /// an item on stack contains left brace with an optional tagName
  class Stack {
    constructor() {
      this.stack = []
    }
    get length() {
      return this.stack.length;
    }
    /// tag can be null, brace shouldn't be null
    push(tag, brace) {
      if (! brace) {
        throw new Error("Brace should not be null");
      }

      this.stack.push({
        tag: tag,
        brace: brace
      })
    }
    pop() {
      let top = this.top();
      if (top) {
        this.stack.pop();
      }
      return top;
    }
    top() {
      let item = this.stack[this.stack.length - 1];
      return item || null;
    }
    topTag() {
      let item = this.top();
      return item ? item.tag : null;
    }
    topBrace() {
      let item = this.top();
      return item ? item.brace : null;
    }
  };

  function startState() {
    return {
      braceStack: new Stack(),
      immediateTag : null,
    };
  }

  function copyState(state) {
    let {braceStack, immediateTag} = state;
    let newStack = new Stack();
    newStack.stack = braceStack.stack.slice();
    return {
      immediateTag,
      braceStack: newStack,
    };
  }

  /// call this function to match the tag and advance the
  /// position. Return the tag or null
  function eatTag(stream) {
    let tag = undefined;
    if (! stream.eat(cmdChar)) {
      return null;
    }
    let matched = stream.match(racketIdRegex);
    if (matched === null) {
      tag = stream.eat(";")
      return tag || null;
    }
    return matched[0];
  }

  function stateInComments(state) {
    let stack = state.braceStack;
    if (stack.topTag() === commentTag) {
      return true;
    }
    if (state.immediateTag === commentTag) {
      return true;
    }
    for (let item of stack.stack) {
      if (item.tag === commentTag) {
        return true;
      }
    }
    return false;
  }

  function token(stream, state) {
    let stack = state.braceStack;
    let inComments = stateInComments(state);
    let tokenType = inComments ? "comment" : null;

    if (!inComments && eatTag(stream)) {
      let tag = stream.current();
      let brace = stream.match(/\|?\{/, false);
      if (brace !== null) {
        state.immediateTag = tag;
      }

      if (tag === commentTag) {
        if (brace === null) {
          stream.skipToEnd();
        }
        return 'comment';
      }

      return 'keyword';
    }

    // we can simplify the body of this if condition, but
    // I prefer leave all cases here as a reminder
    if (stream.match(/\|?\{/)) {
      let brace = stream.current();
      // if brace is { and we're in |{ scope, ignore
      if (brace === '{' && stack.topBrace() === '|{') {
        return tokenType;
      }

      // if brace is |{ and we're in { scope, fix it to be {
      if (brace === '|{' && stack.topBrace() === '{') {
        stream.backUp(1);
        return tokenType;
      }

      // track the left brace only when this is not top level scope or
      // about to open a new scope
      if (stack.top() || state.immediateTag) {
        stack.push(state.immediateTag, brace);
        state.immediateTag = null;
      }
      return tokenType;
    }

    // we can simplify the body of this if condition, but
    // I prefer leave all cases here as a reminder
    if (stream.match(/\}\|?/)) {
      let brace = stream.current();
      // if brace is } and we're in scope |{, ignore
      if (brace === '}' && stack.topBrace() === '|{') {
        return tokenType;
      }

      // if brace is }| and we're in scope {, update it to be }
      if (brace === '}|' && stack.topBrace() === '{') {
        brace = '}';
        stream.backUp(1);
      }

      let top = stack.pop();
      if (! leftRightBraceMatch(top.brace, brace)) {
        console.error("left Brace does not match the right one");
      }

      return tokenType;
    }

    // if none of the pattern matches, we just eat
    state.immediateTag = null;
    let chars = `[^(\\|?{)|(}\\|?)|${cmdChar}]`;
    if (stream.eatWhile(new RegExp(chars)) === false) {
      stream.next();
    }

    return tokenType;
  }

  return {
    startState,
    token,
    copyState,
  };
}


CodeMirror.defineMode("pollen", mode);

CodeMirror.defineMode("pollen1", function(cmConfig, modeConfig) {
  var cmdChar = cmConfig['command-char'] || '◊';
  //var racketId = '[' +  "^ \\n(){}\\[\\]\",'`;#|\\\\" + cmdChar + ']';
  var racketId = `[^ \\n(){}\\[\\]",'\`;#|\\\\${cmdChar}]`;
  var racketIdReg = new RegExp(racketId);

  function stackPush(stack, v) {
    stack.push(v);
  }

  function stackPop(stack, v) {
    return stack.pop();
  }

  function stackEmpty(stack) {
    return stack === [];
  }

  function stackTopMatch(stack, v) {
    return (!stackEmpty(stack)) && stack[stack.length-1] === v;
  }

  return {
    'startState': function () {
      return {
        braceStack: [],  // store '|' (for |{) or '{'
        mode: null
      };
    },

    'token' : function (stream, state) {
      //console.log(stream);
      //console.log(state);
      var ch;

      if (stream.eatSpace()) {
        return null;
      }

      // TODO: to clean this, push tag-function and its brace (either { or |{) information
      // on the stack. if current char is in tag function ";", it is in comment.
      // '}' in ";" with "|{" will be matched one more with "|", etc.
      if (state.mode === 'block-comment') {
        ch = stream.next();
        // keep track of left brace in comment only when the comment
        // starts with { (instead of |{)
        if (ch === '{') {
          if (stackTopMatch(state.braceStack, "comment{") || stackTopMatch(state.braceStack, "{")) {
            // if previous open brace is comment{, push this brace.
            // or if the previous is {, which means this brace is in a
            // comment starting with {, push this brace as well.
            stackPush (state.braceStack, '{');
          }
        }
        else if (ch === '}') {
          if (stackTopMatch(state.braceStack, "comment{")) {
            state.mode = false;
          }
          // comment starts with "|{", we need it to match }|
          else if (stackTopMatch(state.braceStack, "comment|") && stream.eat("|")) {
            state.mode = false;
          }
          // always pop the stack
          // assert (state.braceStack is not empty)
          stackPop(state.braceStack);
        }
        return 'comment';
      }
      else {
        ch = stream.next();
        //console.log('char is: ' + ch);
        if (ch === cmdChar) {
          // @; or @;{ or @;|{
          if (stream.eat(";")) {
            if (stream.eat("|") && stream.eat("{")) {
              stackPush(state.braceStack, "comment|");
              state.mode = 'block-comment';
            }
            else if (stream.eat("{")) {
              stackPush(state.braceStack, "comment{");
              state.mode = 'block-comment';
            }
            else {
              stream.skipToEnd();
            }
            return 'comment';
          }
          // @keyword
          else {
            // var tag = '';
            // var letter;

            if (stream.eat("|")) {
              // racket bar quoted identifier, e.g. @|one, two|
              // https://docs.racket-lang.org/guide/symbols.html
              stream.eatWhile(/[^|]/);
              stream.eat("|");
            } else {
              while ((/*letter = */stream.eat(racketIdReg)) != null) {
                // tag += letter;
              }
            }
            return 'keyword';
          }
        }
        else if (ch === '|') {
          if (stream.eat("{")) {
            stackPush(state.braceStack, ch);
          }
        }
        else if (ch === '{') {
          stackPush(state.braceStack, ch);
        }
        else if (ch === '}') {
          if (stackEmpty(state.braceStack)) {
            // mismatched pair
            return 'invalid';
          }
          else if (stackTopMatch(state.braceStack, "{")) {
            stackPop(state.braceStack);
          }
          else if (stackTopMatch(state.braceStack, "|")) {
            if (stream.eat("|")) {
              stackPop(state.braceStack);
            }
          }
        }
      }
      return null;
    }
  };

});
