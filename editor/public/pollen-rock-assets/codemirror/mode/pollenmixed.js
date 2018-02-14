(function(mod) {
  if (typeof exports == "object" && typeof module == "object") // CommonJS
    mod(require("../../lib/codemirror"));
  else if (typeof define == "function" && define.amd) // AMD
    define(["../../lib/codemirror"], mod);
  else // Plain browser env
    mod(CodeMirror);
})(function(CodeMirror) {
"use strict";

CodeMirror.registerHelper("syntaxCheck", "pollenMixed", function(cm, line) {
  console.log("lint.pollenMixed");
  let state = cm.getStateAfter(line, true);
  return (state.schemeState.indentStack == null
          && state.pollenState.braceStack.length == 0);
});

CodeMirror.defineMode("pollenMixed", function(cmConfig, modeConfig) {
  let schemeMode = CodeMirror.getMode(cmConfig, "scheme");
  let pollenMode = CodeMirror.getMode(cmConfig, "pollen");
  let cmdChar = cmConfig['command-char'] || '◊';

  function pollen(stream, state) {
    let ch = stream.peek();
    let style = pollenMode.token(stream, state.pollenState);
    if (style == "keyword"
        && ((ch  == cmdChar && stream.peek() == '(')
            || stream.peek() == '[')) {
      state.token = scheme;
      state.mode = "scheme";
    }
    return style;
  }

  function scheme(stream, state) {
    let ch = stream.peek();
    let style = schemeMode.token(stream, state.schemeState);
    if (state.schemeState.indentStack == null
        && (ch == "]" || ch == ")")) {
      // closed all bracket, ready to switch back to pollen
      state.token = pollen;
      state.mode = "pollen";
    }
    return style;
  }

  return {
    startState: function() {
      let localState = pollenMode.startState();
      return {
        token: pollen,
        mode: "pollen",
        pollenState: localState,
        schemeState: schemeMode.startState()
      };
    },

    token: function(stream, state) {
      return state.token(stream, state);
    },

    copyState: function(state) {
      return {
        token: state.token,
        mode: state.mode,
        pollenState: CodeMirror.copyState(pollenMode, state.pollenState),
        schemeState: CodeMirror.copyState(schemeMode, state.schemeState)
      };
    },

    /* indent racket code */
    indent: function(state, textAfter) {
      if (state.token == scheme) {
        return schemeMode.indent(state.schemeState, textAfter);
      } else {
        return 0;
      }
    },

    /* For turnning on addModeClass option so racket code can use
     * fixed width font */
    innerMode: function(state) {
      if (state.token == scheme) {
        return {
          state: state.schemeState,
          mode: schemeMode
        };
      } else {
        return {
          state: state.pollenMode,
          mode: pollenMode
        };
      }
    }
  };
});
});
