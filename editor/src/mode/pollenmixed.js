import CodeMirror from 'codemirror';
import './pollen';
import 'codemirror/mode/scheme/scheme';

CodeMirror.registerHelper("syntaxCheck", "pollenMixed", function(cm, line) {
  let state = cm.getStateAfter(line, true);
  return (state.schemeState.indentStack === null
          && state.pollenState.braceCount === 0
          && state.pollenState.blockBraceCount === 0);
});

/// get tags path from the top level scope to the given pos
CodeMirror.registerHelper("getTagPath", "pollenMixed", function(cm, pos) {
  let token = cm.getTokenAt(pos);
  return token.state.pollenState.tagStack;
});


CodeMirror.defineMode("pollenMixed", function(config) {
  let schemeMode = CodeMirror.getMode(config, "scheme");
  let pollenMode = CodeMirror.getMode(config, "pollen");
  let cmdChar = config['command-char'] || '◊';
  let scopeChangeRegex = new RegExp(`${cmdChar}\\(`);

  function pollen(stream, state) {
    if (stream.match(scopeChangeRegex, false)) {
      stream.eat(cmdChar);
      state.token = scheme;
      state.mode = "scheme";
      return 'keyword';
    } else {
      return pollenMode.token(stream, state.pollenState);
    }
  }

  function scheme(stream, state) {
    let ch = stream.peek();
    let style = schemeMode.token(stream, state.schemeState);
    if (state.schemeState.indentStack == null
        && (ch === "]" || ch === ")")) {
      // closed all bracket, ready to switch back to pollen
      state.token = pollen;
      state.mode = "pollen";
    }
    return style;
  }

  return {
    startState: function() {
      return {
        token: pollen,
        mode: "pollen",
        pollenState: pollenMode.startState(),
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
      if (state.token === scheme) {
        return schemeMode.indent(state.schemeState, textAfter);
      } else {
        return 0;
      }
    },

    /* For turnning on addModeClass option so racket code can use
     * fixed width font */
    innerMode: function(state) {
      if (state.token === scheme) {
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
