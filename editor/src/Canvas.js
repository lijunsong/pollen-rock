import React, { Component } from 'react';
import { CM } from './Editor';
import * as Api from './Api.js';
import * as Icons from './Icons.js';
import { Map, List, Set } from 'immutable';
import Path from 'path';

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
        <CanvasPanelEntries
          entries={props.entries}
          fileOnClick={props.fileOnClick}
          folderOnClick={props.folderOnClick}
        />
      </div>
    </div>
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

    this.setState({location: res.data.location});
  }
}

/// The whole canvas
class Canvas extends Component {
  constructor(props) {
    super(props);
    this.state = {
      /// The key is path, value is it's children, a list of string.
      /// Same as pollen-rock fs api, if an item ends with /, the item
      /// is a folder
      entries: Map({"/": List(["Loading"])}),
      openFile: null,
      previewOpened: false,
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
      this.setState({entries});
    });
  }

  onClickPreview() {
    this.setState({previewOpened: !this.state.previewOpened});
  }

  renderNonFullscreen() {
    const {openFile, previewOpened} = this.state;

    let editorView;
    if (openFile) {
      editorView = <CM path={openFile} key={openFile}/>;
    } else {
      const entryList = this.state.entries.get("/");
      editorView = <CanvasOverview parent="/" entryList={entryList}/>;
    }

    let preview;
    if (previewOpened) {
      preview = <CanvasPreview path={openFile} />;
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
            fileOnClick={this.fileOnClick.bind(this)}
            folderOnClick={this.folderOnClick.bind(this)}
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
