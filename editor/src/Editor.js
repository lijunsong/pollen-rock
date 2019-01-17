import React, { Component } from 'react';
import ReactCodeMirror from './ReactCodeMirror';
import * as Api from './Api';
import './mode/pollen';
import CodeMirror from 'codemirror';
import 'codemirror/mode/meta';
import * as Notify from './Notify';
import PropTypes from 'prop-types';


/// This map lists commocommon errno and it's message
const errnoMsg = {
  1: "Operation not permitted",
  2: "No such file or directory",
  17: "File exists",
  20: "Not a directory",
  21: "Is a directory",
  30: "Read-only file system",
  116: "The file has been changed on disk",
};

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
      {tree} {props.closingTagSignature}
    </div>
  );
}

EditorFooter.propTypes = {
  tagStack: PropTypes.array.isRequired,
  closingTagSignature: PropTypes.string,
};


class EditorBody extends Component {
  constructor(props) {
    super(props);
    this.state = {
      // pollen tags of the open file
      tags: [],
    };
    this.initContents = null;
    this.mtime = 0;
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
    // don't use mode.mode. Many names may share the same mode (e.g. html uses
    // htmlmixed mode)
    if (mode && mode.name) {
      modeName = mode.name;
    }

    return modeName;
  }

  getCommandChar() {
    let val = this.state.tags['default-command-char'];
    if (val) {
      return val.value;
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
    let res = await Api.saveContents(path, contents, this.mtime);
    if (res.data.errno !== 0) {
      let errno = res.data.errno;
      let msg;
      if (errno in errnoMsg) {
        let errmsg = errnoMsg[errno];
        msg = `Failed to save ${path}: ${errmsg}`;
      } else {
        msg = res.data.message;
      }
      Notify.error(msg);
      return false;
    } else {
      this.mtime = res.data.message;
    }
    this.savedGeneration = thisGen;
    return true;
  }

  async saveAndPreview(path, cm) {
    let saved = false;
    try {
      saved = await this.saveToDisk(path, cm);
    } catch (e) {
      Notify.error(`Failed to save ${path}: ${e}. Server might have terminated.`);
      return;
    }

    if (! saved) {
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

  getTag(name) {
    return this.state.tags[name] || null;
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
    let events = {
      "changes": this.onChanges.bind(this, path)
    };
    if (modeName.includes("pollen")) {
      events["cursorActivity"] = this.props.onCursorActivity;
    } else {
      console.warn("cursorActivity is available only for pollen mode");
    }

    return (
      <div id="EditorBody">
        <ReactCodeMirror value={this.initContents}
                         ref={r => this.editor = r}
                         options={options}
                         events={events}/>
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
      Notify.error(`Failed to load ${path}: ${e}`);
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
      Notify.error(`Error occurs when sending requests: ${err}`);
      return;
    }

    if (contents.data.errno === 0 && 'contents' in contents.data) {
      this.initContents = contents.data.contents;
      /// mtime for consistency checking
      if (! 'mtime' in contents.data) {
        Notify.warning(
          "Consistency check is unavailable. Other clients " +
            "can silently overwrite this document."
        );
      }
      this.mtime = contents.data.mtime || 0;
      console.log(this.mtime);
    } else {
      Notify.error(`${path} contents are not available`);
      return;
    }

    if (config.data.errno === 0) {
      // construct tag map from the list
      let tags = {};
      for (let tag of config.data.tags) {
        tags[tag.name] = tag;
      }
      this.setState({tags,});
    } else {
      Notify.error(`${path} tags are not available`);
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
