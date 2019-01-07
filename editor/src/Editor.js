import React, { Component } from 'react';
import ReactCodeMirror from '@uiw/react-codemirror';
import * as Api from './Api';
import './mode/pollen';
import CodeMirror from 'codemirror';
import 'codemirror/mode/meta';
import SplitPane from 'react-split-pane';
import * as Icons from './Icons';
import PreviewArea from './PreviewArea';
import PropTypes from 'prop-types';


function EditorHeader(props) {
  return (
    <div id="EditorHeader">
      <span id="EditorPath">{props.path}</span>
      {props.children}
    </div>
  );
}


class EditorBody extends Component {
  constructor(props) {
    super(props);
    this.state = {
      // pollen tags of the open file
      tags: [],
    };
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

  async saveToDisk(path, cm) {
    if (cm.isClean(this.savedGeneration)) {
      console.log("No need to save the doc");
      return;
    }

    let thisGen = cm.changeGeneration();
    let contents = cm.getValue();
    let res = await Api.saveContents(path, contents);
    if (res.data.errno !== 0) {
      console.log(`Failed to save contents: ${res.data.message}`);
      return;
    }

    console.log(`Saved ${path}`);
    this.savedGeneration = thisGen;
  }

  handleOnChanges(path, cm, changes) {
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
        this.saveToDisk(path, cm).then(() => {
          // check syntax error and call synxtax check callback
          let doc = cm.doc;
          let mode = doc.getMode();
          let checkFunc = CodeMirror.syntaxCheck[mode.name];
          let checkResult = true;
          if (checkFunc) {
            checkResult = checkFunc(cm, doc.lastLine());
          }
          this.props.onSyntaxCheck(checkResult);
        }).catch(() => {
          throw new Error("Save to disk failed");
        });
      }, 500);
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
      // FIXME: if wanted contains half parens, we'll get exception
      let matched = lineText.match(wanted);
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
    if (! this.initContents) {
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

    return (
      <div id="editorBody">
        <ReactCodeMirror
          value={this.initContents}
          options={options}
          onChanges={this.handleOnChanges.bind(this, path)}
          ref={r => this.editor = r}
        />
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
      console.log(`Failed to load ${path}: ${e}`);
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
      throw new Error(`Error occurs when sending requests`);
    }

    if (contents.data.errno === 0 && 'contents' in contents.data) {
      this.initContents = contents.data.contents;
    } else {
      throw new Error(`${path} contents are not available`);
    }

    if (config.data.errno === 0) {
      this.setState({tags : config.data.tags});
    } else {
      throw new Error(`${path} tags are not available`);
    }

  }

  componentWillUnmount() {
    window.clearTimeout(this.saveTimer);
  }
}

EditorBody.propTypes = {
  path: PropTypes.string.isRequired,
  onSyntaxCheck: PropTypes.func.isRequired,
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
    };

    this.onDragStarted = this.onDragStarted.bind(this);
    this.onDragFinished = this.onDragFinished.bind(this);
    this.onClickDirection = this.onClickDirection.bind(this);
    this.onPreviewSelected = this.onPreviewSelected.bind(this);
    this.onSyntaxCheck = this.onSyntaxCheck.bind(this);
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

  async fetchLocation(path) {
    console.log("Fetch location of " + path);
    let res = await Api.render(path);
    if (res.data.errno !== 0) {
      throw new Error(`Render failed on ${path}`);
    }

    this.setState({location: res.data.location});
  }

  componentDidMount() {
    this.fetchLocation(this.props.path);
  }

  componentDidUpdate(prevProps) {
    let path = this.props.path;
    if (path !== prevProps.path) {
      this.fetchLocation(path);
    }
  }

  renderEditingArea() {
    let className = "";
    if (this.state.dragging) {
      className = "hidePointerEvents";
    }
    return <div id="EditingArea" className={className}>
             <EditorHeader path={this.props.path} >
               <Icons.FullscreenIcon onClick={this.props.onClickFullscreen} />
             </EditorHeader>
             <EditorBody path={this.props.path}
                         key={this.props.path}
                         onSyntaxCheck={this.onSyntaxCheck}
                         ref={r => this.editorBody = r} />
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
                   paneStyle={{overflow: "auto"}}
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
