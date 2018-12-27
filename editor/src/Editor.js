import React, { Component } from 'react';
import ReactCodeMirror from '@uiw/react-codemirror';
import * as Api from './Api';
import './mode/pollen';
import CodeMirror from 'codemirror';
import 'codemirror/mode/meta';



class EditorHeader extends Component {
  render() {
    return (
      <div id="editorHeader"> header here
      </div>
    );
  }
}


class CM extends Component {
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
    if (pos.ch == 0) {
      e.replaceSelection(commandChar);
    } else {
      let lastPos = CodeMirror.Pos(pos.line, pos.ch-1);
      if (e.getRange(lastPos, pos) == commandChar) {
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
    if (res.data.errno != 0) {
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
      this.saveTimer = window.setTimeout(
        this.saveToDisk.bind(this, path, cm),
        500);
    }
  }

  refresh() {
    if (this.editor) {
      this.editor.editor.refresh();
    }
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

    let newState = {};
    if (contents.data.errno == 0 && 'contents' in contents.data) {
      this.initContents = contents.data.contents;
    } else {
      throw new Error(`${path} contents are not available`);
    }

    if (config.data.errno == 0) {
      this.setState({tags : config.data.tags});
    } else {
      throw new Error(`${path} tags are not available`);
    }

  }

  componentWillUnmount() {
    window.clearTimeout(this.saveTimer);
  }
}


/// Editor contains code mirror and a small header for
/// icons. It optionally contains Preview page
class Editor extends Component {
  render() {
    return <div id="Editor">Editor</div>;
  }
}

export { CM, EditorHeader, Editor };
