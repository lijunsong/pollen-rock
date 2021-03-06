import React, { Component } from 'react';
import Navigation from './Nav';
import EditorAndPreview from './EditorAndPreview';
import logo from './icons/logo.svg';
import * as Notify from './Notify';
import 'react-toastify/dist/ReactToastify.css';

function Splash(props) {
  return <div id="Splash">
           <img src={logo} alt="Pollen-rock-logo"/>
         </div>;
}


class App extends Component {
  constructor(props) {
    super(props);
    this.state = {
      /// Current opened file
      openFile: null,
      /// fullscreen
      fullscreen: false,
    };
  }

  onClickFullscreen() {
    this.setState((state, props) => ({
      fullscreen: !state.fullscreen
    }));
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
        <Notify.ToastContainer/>
      </div>
    );
  }

  renderEditor(path, fullscreen) {
    let nav;
    let className = "fullscreen";
    if (! fullscreen) {
      nav = this.renderNavigation();
      className = "";
    }
    return (
      <div id="App" className={className}>
        {nav}
        <EditorAndPreview
          path={path}
          fullscreen={this.state.fullscreen}
          onClickFullscreen={this.onClickFullscreen.bind(this)}/>
        <Notify.ToastContainer/>
      </div>
    );
  }

  render() {
    if (this.state.openFile) {
      let fullscreen = this.state.fullscreen;
      let openFile = this.state.openFile;
      return this.renderEditor(openFile, fullscreen);
    } else {
      return this.renderSplash();
    }
  }
}

export default App;
