import React, { Component } from 'react';
import CodeMirror from '@uiw/react-codemirror';
import * as Api from './Api';


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

  render() {
    return (
      <div id="editorBody">
        <CodeMirror value={this.state.contents} options={{mode: 'pollen'}} />
      </div>
    );
  }

  componentDidMount() {
    this._loadContents();
  }

  _loadContents() {
    Api.getContents(this.props.path).then((res) => {
      if ('contents' in res.data) {
        this.setState({
          path: this.props.path,
          contents: res.data.contents
        });
      } else {
        this.setState({contents: 'Error: Cannot read a dir'});
      }
    });
  }
}

export { CM, EditorHeader };
