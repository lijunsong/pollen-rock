// CodeMirror, copyright (c) by Marijn Haverbeke and others
// Distributed under an MIT license: http://codemirror.net/LICENSE

(function(mod) {
  if (typeof exports == "object" && typeof module == "object") // CommonJS
    mod(require("../../lib/codemirror"));
  else if (typeof define == "function" && define.amd) // AMD
    define(["../../lib/codemirror"], mod);
  else // Plain browser env
    mod(CodeMirror);
})(function(CodeMirror) {
"use strict";

CodeMirror.defineMode("pollen", function() {
  var cmdChar = '◊';
  var racketId = '[' +  "^ \\n(){}\\[\\]\",'`;#|\\\\" + cmdChar + ']';
  var racketIdReg = new RegExp(racketId);

  function stackPush(stack, v) {
    stack.push(v);
  }

  function stackPop(stack, v) {
    return stack.pop();
  }

  function stackEmpty(stack) {
    return stack == [];
  }

  function stackTopMatch(stack, v) {
    return (!stackEmpty(stack)) && stack[stack.length-1] == v;
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

      // TODO: to clean this, push tag-function and its brace (either { or |{) information
      // on the stack. if current char is in tag function ";", it is in comment.
      // '}' in ";" with "|{" will be matched one more with "|", etc.
      if (state.mode == 'block-comment') {
        ch = stream.next();
        // keep track of left brace in comment only when the comment
        // starts with { (instead of |{)
        if (ch == '{') {
          if (stackTopMatch(state.braceStack, "comment{") || stackTopMatch(state.braceStack, "{")) {
            // if previous open brace is comment{, push this brace.
            // or if the previous is {, which means this brace is in a
            // comment starting with {, push this brace as well.
            stackPush (state.braceStack, '{');
          }
        }
        else if (ch == '}') {
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
        if (ch == cmdChar) {
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
            var tag = '';
            var letter;

            while ((letter = stream.eat(racketIdReg)) != null) {
              tag += letter;
            }
            return 'keyword';
          }
        }
        else if (ch == '|') {
          if (stream.eat("{")) {
            stackPush(state.braceStack, ch);
          }
        }
        else if (ch == '{') {
          stackPush(state.braceStack, ch);
        }
        else if (ch == '}') {
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
    },
  };

});

});
