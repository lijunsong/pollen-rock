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


  class Stack {
    constructor() {
      this.stack = []
    }
    get length() {
      return this.stack.length;
    }
    push(tag, brace) {
      this.stack.push({
        tag: tag,
        brace: brace
      })
    }
    matchAndPop(rightBrace) {
      let tag = this.currentTag();
      if (! tag) {
        return null;
      }
      if (leftRightBraceMatch(tag.brace, rightBrace)) {
        this.stack.pop();
      }
    }
    currentTag() {
      if (this.stack.length === 0) {
        return null;
      }
      return this.stack[this.stack.length - 1];
    }
    currentBrace() {
      let tag = this.stack[this.stack.length - 1];
      if (! tag) {
        return null;
      }
      return tag.brace;
    }
  };

  function startState() {
    return {
      braceStack: new Stack(),
      // a cache for looking back
      currentTagName: null,
      // remember the end pos of <cmd> (because syntax does not allow
      // space between cmd components)
      cmdEnds: -1,
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

  function _eatBrace(stream, brace) {
    let matched = stream.match(brace);
    if (matched !== null) {
      return matched[0];
    }
    return null;
  }

  function eatLeftSimpleBrace(stream) {
    return _eatBrace(stream, /\{/);
  }
  function eatRightSimpleBrace(stream) {
    return _eatBrace(stream, /\}/);
  }
  function eatLeftBlockBrace(stream) {
    return _eatBrace(stream, /\|\{/);
  }
  function eatRightBlockBrace(stream) {
    return _eatBrace(stream, /\|\}/);
  }

  function token(stream, state) {
    let stack = state.braceStack;
    let inComments = false;
    if (state.currentTagName === commentTag) {
      inComments = true;
    }

    if (! inComments && eatTag(stream)) {
      let tag = stream.current();
      state.currentTagName = tag;
      state.cmdEnds = stream.column() + tag.length;
      if (tag === commentTag) {
        return 'comment';
      }

      return 'keyword';
    }

    if (eatLeftSimpleBrace(stream)) {
      let brace = '{';
      // test case ◊func[...]{
      if (state.cmdEnds === stream.column() || stack.currentBrace() === '{') {
        stack.push(state.currentTagName, brace);
      }

      return inComments ? "comment" : null;
    }

    if (eatLeftBlockBrace(stream)) {
      let brace = stream.current();
      // test case ◊func[...]|{
      if (state.cmdEnds === stream.column() || stack.currentBrace() === '|{') {
        stack.push(state.currentTagName, brace);
        return inComments ? "comment" : null;
      } else {
        // backup the stream to treat the first char of '|{' similarly
        // to other chars
        stream.backUp(brace.length);
      }
    }

    let currentBrace = stack.currentBrace();
    if ((currentBrace === '{' && _eatBrace(stream, /\}/))
        || (currentBrace === '|{' && _eatBrace(stream, /\}\|/))) {
      let brace = stream.current();
      stack.matchAndPop(brace);
      // go out of scope, sync currentTagName now
      state.currentTagName = null;
      let tag = stack.currentTag();
      if (tag) {
        state.currentTagName = tag.tag;
      }
      return inComments ? "comment" : null;
    }

    stream.next();
    return inComments ? "comment" : null;
  }

  return {
    startState,
    token,
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
