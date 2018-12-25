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
      path: null
    };
  }

  findMode(path) {
    if (! path) {
      return modeName;
    }

    const modes = {
      pm: 'pollen',
      rkt: 'scheme',
    };
    let modeName = 'pollen';

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

  render() {
    const modeName = this.findMode(this.props.path);
    return (
      <div id="editorBody">
        <ReactCodeMirror value={this.state.contents}
                         options={{mode: modeName, lineWrapping: true}} />
      </div>
    );
  }

  componentDidMount() {
    this._loadContents();
  }

  _loadContents() {
    let path = this.props.path;
    Api.getContents(path).then((res) => {
      if ('contents' in res.data) {
        this.setState({
          path: path,
          contents: res.data.contents
        });
      } else {
        this.setState({contents: 'Error: Cannot read a dir'});
      }
    });
  }
}

export { CM, EditorHeader };
