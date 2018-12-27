import React, { Component } from 'react';
import Navigation from './Nav';
import { Editor } from './Editor';
import * as Api from './Api.js';
import * as Icons from './Icons.js';
import { Map, List, Set } from 'immutable';
import Path from 'path';
import Split from 'react-split';


function Splash(props) {
  return <div id="Splash">Splash</div>;
}


class App extends Component {
  constructor(props) {
    super(props);
    this.state = {
      /// Current opened file
      openFile: null,
    };
  }

  renderNavigation() {
    return (
      <Navigation
        onClickFile={(openFile) => this.setState({openFile})}
      />
    );
  }

  renderSplash() {
    return (
      <div id="App">
        {this.renderNavigation()}
        <Splash />
      </div>
    );
  }

  renderEditor(path) {
    return (
      <div id="App">
        {this.renderNavigation()}
        <Editor path={path} key={path} />
      </div>
    );
  }

  render() {
    if (this.state.openFile) {
      return this.renderEditor(this.state.openFile);
    } else {
      return this.renderSplash();
    }
  }
}

export default App;
