import React, { Component } from 'react';
import ReactCodeMirror from '@uiw/react-codemirror';
import * as Api from './Api';
import './mode/pollen';
import CodeMirror from 'codemirror';
import 'codemirror/mode/meta';
import SplitPane from 'react-split-pane';
import * as Icons from './Icons';
import PreviewArea from './PreviewArea';
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



/// Editor contains code mirror and a small header for
/// icons. It optionally contains Preview page
class Editor extends Component {
  constructor(props) {
    super(props);
    this.state = {
      splitDirection: 'vertical',
      /// this state is needed for SplitPanel's drag event. Because
      /// SplitPanel doesn't set pointer-events to none, I have to
      /// set it on dragging here. Making it in state may not be
      /// necessary.
      dragging: false,
      /// file location to send to preview to load
      location: null,
      /// tag depth
      tagStack: [],
    };

    this.onDragStarted = this.onDragStarted.bind(this);
    this.onDragFinished = this.onDragFinished.bind(this);
    this.onClickDirection = this.onClickDirection.bind(this);
    this.onPreviewSelected = this.onPreviewSelected.bind(this);
    this.onSyntaxCheck = this.onSyntaxCheck.bind(this);
    this.onCursorActivity = this.onCursorActivity.bind(this);

    this.cursorTokenTimer = null;
  }

  onClickDirection(newDirection) {
    if (this.state.splitDirection !== newDirection) {
      this.setState({splitDirection: newDirection});
    }
  }
  onDragStarted() {
    this.setState({dragging: true});
  }
  onDragFinished() {
    this.setState({dragging: false});

    if (this.editorBody) {
      this.editorBody.refresh();
    }
  }
  /// textOrClear is the text selected, or false for selection clear up
  onPreviewSelected(textOrClear) {
    if (this.editorBody) {
      if (textOrClear === false) {
        this.editorBody.clearSyncText();
      }
      this.editorBody.markSyncText(textOrClear);
    }
  }

  onSyntaxCheck(result) {
    if (result === true) {
      this.previewArea.reload();
    } else {
      console.log("Syntax error. No preview reload");
    }
  }

  /// On every cursor move, compare tagStack. Because calling getTokenAt
  /// is too expensive (performance slows down quite a bit if holding down
  /// down key), we delay the actual action to avoid unnecessary parsing
  onCursorActivity(cm) {
    // XXX: cm here initially might be pollen mode and then later
    // changes to the correct mode. So the best bet here is to prevent
    // this handler from being passed to CodeMirror at parent
    // component
    window.clearTimeout(this.cursorTokenTimer);
    this.cursorTokenTimer = window.setTimeout(() => {
      let pos = cm.getCursor();
      let token = cm.getTokenAt(pos);
      const newStack = token.state.braceStack.stack;
      const oldStack = this.state.tagStack;
      if (oldStack.map(e=>e.tag).join(",") !== newStack.map(e=>e.tag).join(",")) {
        this.setState({tagStack: newStack});
      }
    }, 1000);
  }

  async fetchLocation(path) {
    console.log("Fetch location of " + path);
    let res = await Api.render(path);
    if (res.data.errno !== 0) {
      console.error(`Render failed on ${path}`);
      return;
    }

    this.setState({location: res.data.location});
  }

  componentDidMount() {
    this.fetchLocation(this.props.path);
  }

  componentDidUpdate(prevProps, prevState) {
    let path = this.props.path;
    if (path !== prevProps.path) {
      this.fetchLocation(path);
    }

    // refresh the editor only when we're dragging
    if (prevState.dragging !== this.state.dragging && this.editorBody) {
      console.log("refresh the editor for window change");
      this.editorBody.refresh();
    }
  }

  renderEditingArea() {
    let className = "";
    if (this.state.dragging) {
      className = "hidePointerEvents";
    }

    return <div id="EditingArea" className={className}>
             <EditorHeader path={this.props.path} >
               <Icons.IconFullscreen className="clickable"
                                     onClick={this.props.onClickFullscreen} />
             </EditorHeader>
             <EditorBody path={this.props.path}
                         key={this.props.path}
                         onSyntaxCheck={this.onSyntaxCheck}
                         onCursorActivity={this.onCursorActivity}
                         ref={r => this.editorBody = r} />
             <EditorFooter tagStack={this.state.tagStack} />
           </div>;
  }

  /// render the preview area with a wrapper that hides
  /// pointerEvents when necessary. Because the div is just
  /// a wrapper, so we use inline style here
  renderPreviewArea() {
    let className = "";
    if (this.state.dragging) {
      className = "hidePointerEvents";
    }
    let style = {
      width: "100%",
      height: "100%",
    };
    return (
      <div className={className} style={style}>
        <PreviewArea location={this.state.location}
                     ref={r => this.previewArea = r}
                     onClickDirection={this.onClickDirection}
                     onTextSelected={this.onPreviewSelected} />
      </div>
    );
  }

  /// Render the Editor and preview area based on fullscreen states
  /// We use SplitPane to split editor and preview
  render() {
    let className = "nonFullscreen";
    let direction = this.state.splitDirection;
    let previewSize = "50%";

    if (this.props.fullscreen) {
      className = "fullscreen";
      previewSize = "0";
    }
    return (
      <div id="Editor" className={className}>
        <SplitPane split={direction}
                   primary="second"
                   pane2Style={{overflow: "auto"}}
                   onDragFinished={this.onDragFinished}
                   onDragStarted={this.onDragStarted}
                   size={previewSize}>
          {this.renderEditingArea()}
          {this.renderPreviewArea()}
        </SplitPane>
      </div>
    );
  }
}

Editor.propTypes = {
  path: PropTypes.string.isRequired,
  fullscreen: PropTypes.bool.isRequired,
  onClickFullscreen: PropTypes.func.isRequired,
};

export { Editor };
