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
      contents: "Loading...",
      path: null,
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

  render() {
    const modeName = this.findMode(this.props.path);
    const options = {
      mode: modeName,
      lineWrapping: true,
      extraKeys: {
        "'@'": this.insertCommandCharHandler.bind(this)
      }
    };
    return (
      <div id="editorBody">
        <ReactCodeMirror value={this.state.contents}
                         options={options} />
      </div>
    );
  }

  componentDidMount() {
    const path = this.props.path;

    this._loadContents(path).then(e => {
      console.log(`Successfully loaded config of ${path}`);
    }).catch(e => {
      console.log(`Failed to load ${path}: ${e}`);
    });
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
      newState.contents = contents.data.contents;
      newState.path = path;
    } else {
      throw new Error(`${path} contents are not available`);
    }

    if (config.data.errno == 0) {
      newState.tags = config.data.tags;
    } else {
      throw new Error(`${path} tags are not available`);
    }

    console.log(newState);

    this.setState({...newState});
  }
}

export { CM, EditorHeader };
