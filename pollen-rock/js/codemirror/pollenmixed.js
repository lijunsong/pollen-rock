(function(mod) {
  if (typeof exports == "object" && typeof module == "object") // CommonJS
    mod(require("../../lib/codemirror"));
  else if (typeof define == "function" && define.amd) // AMD
    define(["../../lib/codemirror"], mod);
  else // Plain browser env
    mod(CodeMirror);
})(function(CodeMirror) {
"use strict";

CodeMirror.defineMode("pollenMixed", function(config) {
  let schemeMode = CodeMirror.getMode(config, "scheme");
  let pollenMode = CodeMirror.getMode(config, "pollen");

  function pollen(stream, state) {
    let ch = stream.peek();
    let style = pollenMode.token(stream, state.pollenState);
    if ((ch  == '◊' && stream.peek() == '(')
        || (style == "keyword" && stream.peek() == '[')) {
      state.token = scheme;
      state.mode = "pollen";
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
      state.mode = "scheme";
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

    indent: function(state, textAfter) {
      if (state.token == scheme) {
        return schemeMode.indent(state.schemeState, textAfter);
      } else {
        return 0;
      }
    }
  };
});
});
