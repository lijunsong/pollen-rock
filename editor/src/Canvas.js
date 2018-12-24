import React, { Component } from 'react';
import { CM } from './Editor';
import * as Api from './Api.js';


/*class CanvasHeader extends Component {
  render() {
  }
  }*/

function CanvasHeader(props) {
  return (
    <div id="canvasHeader">{props.path}</div>
  );
}

/// rendering all entry items
function CanvasPanelContents(props) {
  const entries = props.entries.map((e, i) => {
    if (e.endsWith("/")) {
      return <div className="isDir entry"
              key={i}>{e}</div>;
    } else {
      return <div className="isFile entry"
                 key={i}
                 onClick={() => { props.onClick(e); }}>
                {e}
              </div>;
    }
  });

  return (
    <div className="entries">{entries}</div>
  );
}

/// Render the left panel bar when Canvas is not in fullscreen
function CanvasPanel(props) {
  return (
    <div id="canvasPanel">
      <div id="canvasPanelHeader">Entries
      </div>
      <div id="canvasPanelContents">
        <CanvasPanelContents
          entries={props.entries}
          onClick={props.entryOnClick}
        />
      </div>
    </div>
  );
}

class Canvas extends Component {
  constructor(props) {
    super(props);
    this.state = {
      entries: ['Loading'],
      openFile: null
    };
  }

  renderFullscreen() {
    return (
      <div id="canvas">
        <div className="container">
          <div className="row">
            <div className="two columns side">leftbar</div>
            <div id="editorFrame" className="eight columns">
              <CM path={this.state.openFile}/>
            </div>
            <div className="two columns side">rightbar</div>
          </div>
        </div>
      </div>
    );
  }

  entryOnClick(filePath) {
    this.setState({openFile: filePath});
  }

  renderNonFullscreen() {
    const openFile = this.state.openFile;

    return (
      <div id="canvas">
        <CanvasHeader path={openFile} />
        <div className="sideBySideWrapper">
          <CanvasPanel
            entries={this.state.entries}
            entryOnClick={this.entryOnClick.bind(this)}
          />
          <CM path={openFile} key={openFile}/>
        </div>
      </div>
    );
  }

  componentDidMount() {
    this._loadEntryData();
  }

  _loadEntryData() {
    Api.getContents("/").then((res) => {
      this.setState({entries: res.data.items});
    });
  }

  render() {
    return this.renderNonFullscreen();
  }
}

export default Canvas;
