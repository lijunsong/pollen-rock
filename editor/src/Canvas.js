import React, { Component } from 'react';
import { CM } from './Editor';
import * as Api from './Api.js';
import * as Icons from './Icons.js';
import { Map, List } from 'immutable';
import Path from 'path';

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
                  key={i}>{Icons.arrowRight} {e}</div>;
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

/// The entry tab of the panel
class CanvasPanelEntries extends Component {
  _getFileOnClick(path) {
    return () => this.props.fileOnClick(path);
  }
  _getFolderOnClick(path) {
    return () => {
      console.log("folder click " + path);
      this.props.folderOnClick(path);
    };
  }
  /// recursively render folders and it's children
  _renderFolder(parentPath, folderName) {
    const fullPath = Path.join(parentPath, folderName);
    const folder = <span onClick={this._getFolderOnClick(fullPath)}>
                     {Icons.arrowRight} {folderName}
                   </span>;
    const children = this.props.entries.get(fullPath);
    let childrenView = "";
    if (children) {
      childrenView = this._renderEntryList(fullPath, children);
    }
    return <div className="isDir entry" key={fullPath}>
             {folder}
             <div className="spacer">{childrenView}</div>
           </div>;
  }

  /// render a single file entry
  _renderFile(parentPath, fileName) {
    const fullPath = Path.join(parentPath, fileName);
    return <div className="isFile entry" key={fullPath}
                onClick={this._getFileOnClick((fullPath))}>
             {fileName}
           </div>;
  }

  /// Return a list of rendered entries
  _renderEntryList(parentPath, list) {
    return list.map((e) => {
      if (e.endsWith("/")) {
        return this._renderFolder(parentPath, e);
      } else {
        return this._renderFile(parentPath, e);
      }
    });
  }

  render() {
    const parentPath = "/";
    const list = this.props.entries.get(parentPath);
    const renderedList = this._renderEntryList(parentPath, list);

    if (renderedList.isEmpty()) {
      renderedList = "No files";
    }
     return <div className="entries">{renderedList}</div>;
  }
}

/// Render the left panel bar when Canvas is not in fullscreen
///
///        <CanvasPanelContents
///          entries={props.entries}
///          onClick={props.entryOnClick}
///        />
function CanvasPanel(props) {
  return (
    <div id="canvasPanel">
      <div id="canvasPanelHeader">Entries
      </div>
      <div id="canvasPanelContents">
        <CanvasPanelEntries
          entries={props.entries}
          fileOnClick={props.fileOnClick}
          folderOnClick={props.folderOnClick}
        />
      </div>
    </div>
  );
}

class Canvas extends Component {
  constructor(props) {
    super(props);
    this.state = {
      /// entries key is path, value is it's children, a list of string.
      /// Same as pollen-rock fs api, if item ends with /, the item is
      /// a folder
      entries: Map({"/": List(["Loading"])}),
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

  fileOnClick(filePath) {
    this.setState({openFile: filePath});
  }

  folderOnClick(folderPath) {
    Api.getContents(folderPath).then((res) => {
      console.log(`set ${folderPath} in the entries map`);
      const entries = this.state.entries.set(folderPath, List(res.data.items));
      console.log(entries);
      this.setState({entries});
    });
  }

  renderNonFullscreen() {
    const openFile = this.state.openFile;

    return (
      <div id="canvas">
        <CanvasHeader path={openFile} />
        <div className="sideBySideWrapper">
          <CanvasPanel
            entries={this.state.entries}
            fileOnClick={this.fileOnClick.bind(this)}
            folderOnClick={this.folderOnClick.bind(this)}
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
      const oldEntries = this.state.entries;
      const newEntries = oldEntries.set("/", List(res.data.items));
      this.setState({entries: newEntries});
    });
  }

  render() {
    return this.renderNonFullscreen();
  }
}

export default Canvas;
