import React, { Component } from 'react';
import ReactCodeMirror from '@uiw/react-codemirror';
import * as Api from './Api';
import './mode/pollen';
import CodeMirror from 'codemirror';
import 'codemirror/mode/meta';
import { notify } from './Notify';
import PropTypes from 'prop-types';


function EditorHeader(props) {
  return (
    <div id="EditorHeader">
      <span id="EditorPath">{props.path}</span>
      {props.children}
    </div>
  );
}

EditorHeader.propTypes = {
  path: PropTypes.string.isRequired,
};


function EditorFooter(props) {
  let stack = props.tagStack || [];
  let tree = stack.map(tag => tag.tag).join(" > ");
  return (
    <div id="EditorFooter">
      {tree}
    </div>
  );
}

EditorFooter.propTypes = {
  tagStack: PropTypes.array.isRequired,
};


class EditorBody extends Component {
  constructor(props) {
    super(props);
    this.state = {
      // pollen tags of the open file
      tags: [],
    };
    this.initContents = null;
  }

  findMode(path) {
    let modeName = 'pollen';
    if (! path) {
      return modeName;
    }

    const modes = {
      pm: 'pollen',
      rkt: 'scheme',
    };

    const pathComponents = path.split('.');
    let ext = pathComponents.pop();
    if (ext === "pp" || ext === "p") {
      ext = pathComponents.pop();
    }

    if (modes[ext]) {
      return modes[ext];
    }

    const mode = CodeMirror.findModeByExtension(ext);
    // Use name here because ReactCodeMirror looks for mode by name
    // (name is defined in codemirror/mode/meta.js), not the mode string
    if (mode && mode.name) {
      modeName = mode.name;
    }

    return modeName;
  }

  getCommandChar() {
    for (var tag of this.state.tags) {
      if (tag.name === 'default-command-char') {
        return tag.value;
      }
    }
    return '@';
  }

  insertCommandCharHandler(e) {
    let pos = e.getCursor();
    let commandChar = this.getCommandChar();
    if (pos.ch === 0) {
      e.replaceSelection(commandChar);
    } else {
      let lastPos = CodeMirror.Pos(pos.line, pos.ch-1);
      if (e.getRange(lastPos, pos) === commandChar) {
        e.replaceRange('@', lastPos, pos);
      } else {
        e.replaceSelection(commandChar);
      }
    }
  }

  /// save contents to remote. This method returns true
  /// when contents are sync on both sides; false otherwise
  async saveToDisk(path, cm) {
    if (cm.isClean(this.savedGeneration)) {
      return true;
    }

    let thisGen = cm.changeGeneration();
    let contents = cm.getValue();
    let res = await Api.saveContents(path, contents);
    if (res.data.errno !== 0) {
      notify.error(`Failed to save contents: ${res.data.message}`);
      return false;
    }

    this.savedGeneration = thisGen;
    return true;
  }

  async saveAndPreview(path, cm) {
    try {
      await this.saveToDisk(path, cm);
    } catch (e) {
      notify.error(`Failed to save ${path}: ${e}`);
      return;
    }

    // check syntax error and call synxtax check callback
    let doc = cm.doc;
    let mode = doc.getMode();
    let checkFunc = CodeMirror.syntaxCheck[mode.name];
    let checkResult = true;
    if (checkFunc) {
      checkResult = checkFunc(cm, doc.lastLine());
    }
    this.props.onSyntaxCheck(checkResult);
  }

  onChanges(path, cm) {
    if (! path) {
      return;
    }

    if (this.savedGeneration < 0) {
      // the first time this function is called. This happens
      // only when we load the doc the first time. Mark clean
      // now, and later check clean in saver
      this.savedGeneration = cm.changeGeneration();
    } else {
      // cancel previous timer and schedule a new one
      window.clearTimeout(this.saveTimer);
      this.saveTimer = window.setTimeout(() => {
        this.saveAndPreview(path, cm);
      }, 1000);
    }
  }

  refresh() {
    if (this.editor) {
      this.editor.editor.refresh();
    }
  }

  markSyncText(wanted) {
    if (! this.editor) {
      return;
    }

    /// cache already highlighted markers into this.syncMarkers
    this.clearSyncText();

    /// now find the wanted text, and highlight them
    let matchedPos = [];
    let doc = this.editor.editor.getDoc();
    this.editor.editor.eachLine((line) => {
      let lineText = line.text;
      let wantedRegex = new RegExp(wanted, "i");
      let matched = lineText.match(wantedRegex);
      if (matched) {
        let lineNumber = doc.getLineNumber(line);
        let fromPos = {line: lineNumber, ch: matched.index};
        let toPos = {line: lineNumber, ch: matched.index + wanted.length};
        matchedPos.push({fromPos: fromPos, toPos: toPos});
      }
    });
    if (matchedPos.length !== 0) {
      matchedPos.forEach(obj => {
        let marker = doc.markText(obj.fromPos, obj.toPos, {
          className: "previewTextFound"
        });
        this.syncMarkers.push(marker);
      });

      this.editor.editor.scrollIntoView(matchedPos[0].fromPos);
    }
  }

  clearSyncText() {
    if (this.syncMarkers) {
      this.syncMarkers.forEach(m => {
        m.clear();
      });
    }
    this.syncMarkers = [];
  }

  render() {
    if (this.initContents === null) {
      return <p>Loading...</p>;
    }

    const path = this.props.path;
    const modeName = this.findMode(path);
    const options = {
      mode: modeName,
      lineNumbers: true,
      lineWrapping: true,
      extraKeys: {
        "'@'": this.insertCommandCharHandler.bind(this)
      }
    };
    let props = {
      value: this.initContents,
      options: options,
      onChanges: this.onChanges.bind(this, path),
      ref: r => this.editor = r,
    };
    if (modeName.includes("pollen")) {
      props.onCursorActivity = this.props.onCursorActivity;
    } else {
      console.warn("cursorActivity is available only for pollen mode");
    }

    return (
      <div id="EditorBody">
        <ReactCodeMirror {...props} />
      </div>
    );
  }

  /// Save a lot of data that are not needed in render.
  /// This saves a lot of unnecessary render calls
  componentDidMount() {
    const path = this.props.path;

    this._loadContents(path).then(e => {
      console.log(`Successfully loaded config of ${path}`);
    }).catch(e => {
      notify.error(`Failed to load ${path}: ${e}`);
    });

    // reference to the timeout for autosave
    this.saveTimer = null;
    // the codemirror generation of which contents have been saved to disk
    this.savedGeneration = -1;
  }

  async _loadContents (path) {
    let contents;
    let config;

    try {
      let waitContents = Api.getContents(path);
      let waitConfig = Api.getConfig(path);

      contents = await waitContents;
      config = await waitConfig;
    } catch (err) {
      notify.error(`Error occurs when sending requests`);
      return;
    }

    if (contents.data.errno === 0 && 'contents' in contents.data) {
      this.initContents = contents.data.contents;
    } else {
      notify.error(`${path} contents are not available`);
      return;
    }

    if (config.data.errno === 0) {
      this.setState({tags : config.data.tags});
    } else {
      notify.error(`${path} tags are not available`);
    }

  }

  componentWillUnmount() {
    window.clearTimeout(this.saveTimer);
    window.clearTimeout(this.cursorTokenTimer);
  }
}

EditorBody.propTypes = {
  path: PropTypes.string.isRequired,
  onSyntaxCheck: PropTypes.func.isRequired,
  onCursorActivity: PropTypes.func.isRequired,
};

export { EditorHeader, EditorBody, EditorFooter };
