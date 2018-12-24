import React, { Component } from 'react';
import { CM } from './Editor';
import * as Api from './Api.js';
import * as Icons from './Icons.js';
import { Map, List, Set } from 'immutable';
import Path from 'path';

function CanvasHeader(props) {
  return (
    <div id="canvasHeader">{props.path}</div>
  );
}

/// The entry tab of the panel
class CanvasPanelEntries extends Component {
  constructor(props) {
    super(props);
    this.state = {
      // a set of paths that have expanded
      expanded: Set()
    };
  }

  _getFileOnClick(path) {
    return () => this.props.fileOnClick(path);
  }

  _getFolderOnClick(path) {
    return () => {
      let expanded;
      if (this.state.expanded.has(path)) {
        expanded = this.state.expanded.delete(path);
      } else {
        this.props.folderOnClick(path);
        expanded = this.state.expanded.add(path);
      }
      this.setState({expanded});
    };
  }

  /// recursively render folders and it's children
  _renderFolder(parentPath, folderName) {
    const fullPath = Path.join(parentPath, folderName);
    let arrow = Icons.arrowRight;
    // render all children if this folder is expanded
    let childrenView = "";
    if (this.state.expanded.has(fullPath)) {
      arrow = Icons.arrowDown;
      const children = this.props.entries.get(fullPath);
      if (children) {
        childrenView = (
          <div className="spacer">
            {this._renderEntryList(fullPath, children)}
          </div>
        );
      }
    }

    const folder = (
      <span className="entry" onClick={this._getFolderOnClick(fullPath)}>
        {arrow}{folderName}
      </span>
    );

    return <div className="isDir" key={fullPath}>
             {folder}
             {childrenView}
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
