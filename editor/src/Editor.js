import React, { Component } from 'react';
import ReactCodeMirror from '@uiw/react-codemirror';
import * as Api from './Api';
import './mode/pollen';
import CodeMirror from 'codemirror';
import 'codemirror/mode/meta';
import Split from 'react-split';
import * as Icons from './Icons';


function EditorHeader(props) {
  return (
    <div id="EditorHeader">
      <span id="EditorPath">{props.path}</span>
      {props.children}
    </div>
  );
}

class EditorBody extends Component {
  constructor(props) {
    super(props);
    this.state = {
      // pollen tags of the open file
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

  async saveToDisk(path, cm) {
    if (cm.isClean(this.savedGeneration)) {
      console.log("No need to save the doc");
      return;
    }

    let thisGen = cm.changeGeneration();
    let contents = cm.getValue();
    let res = await Api.saveContents(path, contents);
    if (res.data.errno != 0) {
      console.log(`Failed to save contents: ${res.data.message}`);
      return;
    }

    console.log(`Saved ${path}`);
    this.savedGeneration = thisGen;
  }

  handleOnChanges(path, cm, changes) {
    if (! path) {
      return;
    }

    if (this.savedGeneration < 0) {
      // the first time this function is called. This happens
      // only when we load the doc the first time. Mark clean
      // now, and later check clean in saver
      this.savedGeneration = cm.changeGeneration();
    } else {
      // cancel previous timer and schedule a new one
      window.clearTimeout(this.saveTimer);
      this.saveTimer = window.setTimeout(
        this.saveToDisk.bind(this, path, cm),
        500);
    }
  }

  refresh() {
    if (this.editor) {
      this.editor.editor.refresh();
    }
  }

  render() {
    if (! this.initContents) {
      return <p>Loading...</p>;
    }

    const path = this.props.path;
    const modeName = this.findMode(path);
    const options = {
      mode: modeName,
      lineNumbers: true,
      lineWrapping: true,
      extraKeys: {
        "'@'": this.insertCommandCharHandler.bind(this)
      }
    };

    return (
      <div id="editorBody">
        <ReactCodeMirror
          value={this.initContents}
          options={options}
          onChanges={this.handleOnChanges.bind(this, path)}
          ref={r => this.editor = r}
        />
      </div>
    );
  }

  /// Save a lot of data that are not needed in render.
  /// This saves a lot of unnecessary render calls
  componentDidMount() {
    const path = this.props.path;

    this._loadContents(path).then(e => {
      console.log(`Successfully loaded config of ${path}`);
    }).catch(e => {
      console.log(`Failed to load ${path}: ${e}`);
    });

    // reference to the timeout for autosave
    this.saveTimer = null;
    // the codemirror generation of which contents have been saved to disk
    this.savedGeneration = -1;
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
      this.initContents = contents.data.contents;
    } else {
      throw new Error(`${path} contents are not available`);
    }

    if (config.data.errno == 0) {
      this.setState({tags : config.data.tags});
    } else {
      throw new Error(`${path} tags are not available`);
    }

  }

  componentWillUnmount() {
    window.clearTimeout(this.saveTimer);
  }
}


class PreviewArea extends Component {
  constructor(props) {
    super(props);
    this.state = {
      location: null,
    };
  }

  async renderRequest(path) {
    let res = await Api.render(path);
    if (res.data.errno != 0) {
      throw new Error(`Render failed on ${path}`);
    }

    this.setState({location: res.data.location});
  }

  componentDidMount() {
    let path = this.props.path;

    if (!path) {
      return;
    }

    this.renderRequest(path).then(() => {
      console.log(`Successfully render ${path}`);
    }).catch((e) => {
      console.log(`Failed to render ${path}: ${e}`);
    });
  }

  renderLoading() {
    return <div id="PreviewArea">
             "Loading";
           </div>;
  }

  renderPreview(location) {
    let url = `${Api.remote}/${location}`;
    return <div id="PreviewArea">
             <iframe sandbox="allow-scripts" src={url} />
           </div>;
  }

  render() {
    if (this.state.location) {
      return this.renderPreview(this.state.location);
    } else {
      return this.renderLoading();
    }
  }
}


/// Editor contains code mirror and a small header for
/// icons. It optionally contains Preview page
class Editor extends Component {
  constructor(props) {
    super(props);
    this.state = {
      splitDirection: 'horizontal',
    };

    this.onDragEnd = this.onDragEnd.bind(this);
    this.onClickDirection = this.onClickDirection.bind(this);
  }
  onClickDirection(newDirection) {
    if (this.state.splitDirection != newDirection) {
      this.setState({splitDirection: newDirection});
    }
  }
  onDragEnd() {
    if (this.editorBody) {
      this.editorBody.refresh();
    }
  }
  elementStyle(dimension, size, gutterSize) {
    return {
      'flex-basis': 'calc(' + size + '% - ' + gutterSize + 'px)',
    };
  }
  gutterStyle(dimension, size) {
    return {
      'flex-basis': size + 'px'
    };
  }
  renderEditingArea(icons) {
    return <div id="EditingArea">
             <EditorHeader path={this.props.path} >
               {icons}
             </EditorHeader>
             <EditorBody path={this.props.path}
                         ref={r => this.editorBody = r} />
           </div>;
  }
  renderFullscreen() {
    return <div id="Editor" className="fullscreen">
             {this.renderEditingArea([
               <Icons.FullscreenIcon
                 key={4}
                 onClick={this.props.onClickFullscreen}
               />,
             ])}
           </div>;
  }
  renderNonFullscreen() {
    let icons = [
      <Icons.HorizontalSplitIcon
        key={1}
        onClick={() => this.onClickDirection('vertical')} />,
      <Icons.VerticalSplitIcon
        key={2}
        onClick={() => this.onClickDirection('horizontal')} />,
      <Icons.FullscreenIcon
        key={3}
        onClick={this.props.onClickFullscreen}
      />,
    ];
    let direction = this.state.splitDirection;
    // Split does not handle refresh so well, so we use flex-style,
    // passing in elementStyle and gutterStyle, and change only Split
    // className
    return <div id="Editor" className="nonFullscreen">
             <Split className={`split-${direction}`} sizes={[50, 50]}
                    onDragEnd={this.onDragEnd}
                    direction={direction}
                    elementStyle={this.elementStyle.bind(this)}
                    gutterStyle={this.gutterStyle.bind(this)}
             >
               {this.renderEditingArea(icons)}
               <PreviewArea path={this.props.path} />
             </Split>
           </div>;
  }
  render() {
    if (this.props.fullscreen) {
      return this.renderFullscreen();
    } else {
      return this.renderNonFullscreen();
    }
  }
}

export { Editor };
