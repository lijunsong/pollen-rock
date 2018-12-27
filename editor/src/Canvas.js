import React, { Component } from 'react';
import { CM } from './Editor';
import * as Api from './Api.js';
import * as Icons from './Icons.js';
import { Map, List, Set } from 'immutable';
import Path from 'path';
import Split from 'react-split';


function CanvasHeader(props) {
  let operations = "";
  if (props.path) {
    operations = <span id="preview" onClick={props.onClickPreview}>{Icons.preview}</span>;
  }
  return (
    <div id="canvasHeader">
      <div id="canvasHeaderPath">{props.path}</div>
      <div id="canvasHeaderOps">{operations}</div>
    </div>
  );
}


/// Components for the left panel bar when canvas is not in fullscreen
function CanvasPanel(props) {
  return (
    <div id="canvasPanel">
      <div id="canvasPanelHeader">Entries</div>
      <div id="canvasPanelContents">
        <CanvasNavigation
          entries={props.entries}
          onClickFile={props.onClickFile}
          onClickFolder={props.onClickFolder}
        />
      </div>
    </div>
  );
}


/// The entry tab of the panel



/// Components of the right side area when Canvas is not fullscreen
class CanvasOverview extends Component {
  render() {
    return (
      <div id="canvasOverview">overview </div>
    );
  }
}


class CanvasPreview extends Component {
  constructor(props) {
    super(props);
    this.state = {
      location: null,
    };
  }
  render() {
    if (this.state.location) {
      return <div className="previewFrame">
               <iframe sandbox="allow-scripts" src={Api.remote + "/" + this.state.location}/>
             </div>;
    } else {
      return <p>Loading...</p>;
    }
  }
  componentDidMount() {
    let path = this.props.path;

    this.renderRequest(path).then(() => {
      console.log(`Successfully render ${path}`);
    }).catch((e) => {
      console.log(`Failed to render ${path}: ${e}`);
    });
  }

  async renderRequest(path) {
    let res = await Api.render(path);
    if (res.data.errno != 0) {
      throw new Error(`Render failed on ${path}`);
    }

    this.setState({location: res.data.location}, ()=>{
      this.props.callAfterShow();
    });
  }
}

/// The whole canvas
class Canvas extends Component {
  constructor(props) {
    super(props);
;
  }

  onClickFile(filePath) {
    this.setState({openFile: filePath});
  }

  onClickFolder(folderPath) {
    Api.getContents(folderPath).then((res) => {
      console.log(`set ${folderPath} in the entries map`);
      const entries = this.state.entries.set(folderPath, List(res.data.items));
      this.setState({entries});
    });
  }

  onClickPreview() {
    this.setState({previewOpened: !this.state.previewOpened});
  }

  renderNonFullscreen() {
    return (
    );
  }

  renderNonFullscreen1() {
    const {openFile, previewOpened} = this.state;

    let editorView;
    if (openFile) {
      editorView = <CM path={openFile} key={openFile}
                       ref={r => this.cm = r}
                   />;
    } else {
      const entryList = this.state.entries.get("/");
      editorView = <CanvasOverview parent="/" entryList={entryList}/>;
    }

    let preview;
    if (previewOpened) {
      preview = <CanvasPreview
                  path={openFile}
                  callAfterShow={() => {
                    if (this.cm) {
                      this.cm.refresh();
                    }
                  }}
                />;
    }

    let canvas = (
      <div id="canvas">
        <CanvasHeader
          path={openFile}
          onClickPreview={this.onClickPreview.bind(this)}
        />
        <div className="sideBySideWrapper">
          <CanvasPanel
            entries={this.state.entries}
            onClickFile={this.onClickFile.bind(this)}
            onClickFolder={this.onClickFolder.bind(this)}
          />
          {editorView}
          {preview}
        </div>
      </div>
    );

    return canvas;
  }

  

  componentDidMount() {
    this._loadEntryData();
  }

  _loadEntryData() {
    Api.getContents("/").then((res) => {
      const list = List(res.data.items);
      const folders = list.filter(f => f.endsWith("/"));
      const files = list.filterNot(f => f.endsWith("/"));
      const finalList = folders.concat(files);

      const newEntries = this.state.entries.set("/", finalList);
      this.setState({entries: newEntries});
    });
  }

  render() {
    return this.renderNonFullscreen();
  }
}

export default Canvas;
