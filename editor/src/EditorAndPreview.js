import React, { Component } from 'react';
import PropTypes from 'prop-types';
import * as Api from './Api';
import SplitPane from 'react-split-pane';
import PreviewArea from './PreviewArea';
import * as Icons from './Icons';
import * as Editor from './Editor';


/// Editor contains code mirror and a small header for
/// icons. It optionally contains Preview page
class EditorAndPreview extends Component {
  constructor(props) {
    super(props);
    this.state = {
      splitDirection: 'vertical',
      /// this state is needed for SplitPanel's drag event. Because
      /// SplitPanel doesn't set pointer-events to none, I have to
      /// set it on dragging here. Making it in state may not be
      /// necessary.
      dragging: false,
      /// file location to send to preview to load
      location: null,
      /// tag depth
      tagStack: [],
    };

    this.onDragStarted = this.onDragStarted.bind(this);
    this.onDragFinished = this.onDragFinished.bind(this);
    this.onClickDirection = this.onClickDirection.bind(this);
    this.onPreviewSelected = this.onPreviewSelected.bind(this);
    this.onSyntaxCheck = this.onSyntaxCheck.bind(this);
    this.onCursorActivity = this.onCursorActivity.bind(this);

    this.cursorTokenTimer = null;
  }

  onClickDirection(newDirection) {
    if (this.state.splitDirection !== newDirection) {
      this.setState({splitDirection: newDirection});
    }
  }
  onDragStarted() {
    this.setState({dragging: true});
  }
  onDragFinished() {
    this.setState({dragging: false});

    if (this.editorBody) {
      this.editorBody.refresh();
    }
  }
  /// textOrClear is the text selected, or false for selection clear up
  onPreviewSelected(textOrClear) {
    if (this.editorBody) {
      if (textOrClear === false) {
        this.editorBody.clearSyncText();
      }
      this.editorBody.markSyncText(textOrClear);
    }
  }

  onSyntaxCheck(result) {
    if (result === true) {
      this.previewArea.reload();
    } else {
      console.log("Syntax error. No preview reload");
    }
  }

  /// On every cursor move, compare tagStack. Because calling getTokenAt
  /// is too expensive (performance slows down quite a bit if holding down
  /// down key), we delay the actual action to avoid unnecessary parsing
  onCursorActivity(cm) {
    // XXX: cm here initially might be pollen mode and then later
    // changes to the correct mode. So the best bet here is to prevent
    // this handler from being passed to CodeMirror at parent
    // component
    window.clearTimeout(this.cursorTokenTimer);
    this.cursorTokenTimer = window.setTimeout(() => {
      let pos = cm.getCursor();
      let token = cm.getTokenAt(pos);
      const newStack = token.state.braceStack.stack;
      const oldStack = this.state.tagStack;
      if (oldStack.map(e=>e.tag).join(",") !== newStack.map(e=>e.tag).join(",")) {
        this.setState({tagStack: newStack});
      }
    }, 1000);
  }

  async fetchLocation(path) {
    console.log("Fetch location of " + path);
    let res = await Api.render(path);
    if (res.data.errno !== 0) {
      console.error(`Render failed on ${path}`);
      return;
    }

    this.setState({location: res.data.location});
  }

  componentDidMount() {
    this.fetchLocation(this.props.path);
  }

  componentDidUpdate(prevProps, prevState) {
    let path = this.props.path;
    if (path !== prevProps.path) {
      this.fetchLocation(path);
    }

    // refresh the editor only when we're dragging
    if (prevState.dragging !== this.state.dragging && this.editorBody) {
      console.log("refresh the editor for window change");
      this.editorBody.refresh();
    }
  }

  renderEditingArea() {
    let className = "";
    if (this.state.dragging) {
      className = "hidePointerEvents";
    }

    let signature;
    if (this.editorBody) {
      let closingTag = this.state.tagStack[this.state.tagStack.length-1];
      signature = this.editorBody.getTag(closingTag);
    }
    return <div id="EditingArea" className={className}>
             <Editor.EditorHeader path={this.props.path} >
               <Icons.IconFullscreen className="clickable"
                                     onClick={this.props.onClickFullscreen} />
             </Editor.EditorHeader>
             <Editor.EditorBody path={this.props.path}
                                key={this.props.path}
                                onSyntaxCheck={this.onSyntaxCheck}
                                onCursorActivity={this.onCursorActivity}
                                ref={r => this.editorBody = r} />
             <Editor.EditorFooter
               tagStack={this.state.tagStack}
               closingTagSignature={signature}
             />
           </div>;
  }

  /// render the preview area with a wrapper that hides
  /// pointerEvents when necessary. Because the div is just
  /// a wrapper, so we use inline style here
  renderPreviewArea() {
    let className = "";
    if (this.state.dragging) {
      className = "hidePointerEvents";
    }
    let style = {
      width: "100%",
      height: "100%",
    };
    return (
      <div className={className} style={style}>
        <PreviewArea location={this.state.location}
                     ref={r => this.previewArea = r}
                     onClickDirection={this.onClickDirection}
                     onTextSelected={this.onPreviewSelected} />
      </div>
    );
  }

  /// Render the Editor and preview area based on fullscreen states
  /// We use SplitPane to split editor and preview
  render() {
    let className = "nonFullscreen";
    let direction = this.state.splitDirection;
    let previewSize = "50%";

    if (this.props.fullscreen) {
      className = "fullscreen";
      previewSize = "0";
    }
    return (
      <div id="Editor" className={className}>
        <SplitPane split={direction}
                   primary="second"
                   pane2Style={{overflow: "auto"}}
                   onDragFinished={this.onDragFinished}
                   onDragStarted={this.onDragStarted}
                   size={previewSize}>
          {this.renderEditingArea()}
          {this.renderPreviewArea()}
        </SplitPane>
      </div>
    );
  }
}

EditorAndPreview.propTypes = {
  path: PropTypes.string.isRequired,
  fullscreen: PropTypes.bool.isRequired,
  onClickFullscreen: PropTypes.func.isRequired,
};

export default EditorAndPreview;
