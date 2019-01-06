import React, { Component } from 'react';
import * as Api from './Api';
import * as Icons from './Icons';
import PropTypes from 'prop-types';


class PreviewArea extends Component {
  constructor(props) {
    super(props);
    this.onClickViewColumn = () => this.props.onClickDirection("vertical");
    this.onClickViewRow = () => this.props.onClickDirection("horizontal");
    this.onLoad = this.onLoad.bind(this);
  }

  onLoad() {
    if (this.iframe) {
      this.installSelectHandler(this.iframe.contentWindow);
    }
  }

  reload() {
    if (! this.iframe) {
      return;
    }

    try {
      this.iframe.contentWindow.location.reload(true);
    } catch (err) {
      console.warn("Failed to reload, likely caused by same-origin policy");
      this.iframe.src += '';
    }
  }

  installSelectHandler(contentWindow) {
    console.log("Install select handler in iframe");
    contentWindow.addEventListener("mouseup", event => {
      let selected = contentWindow.getSelection().toString();
      // contract here is that false means no selection
      this.props.onTextSelected(selected || false);
    });
  }

  renderLoading() {
    return <div id="PreviewArea">
             "Loading";
           </div>;
  }

  renderHeader(location) {
    return <div id="PreviewHeader">
             <span id="PreviewPath">{location}</span>
             <Icons.HorizontalSplitIcon onClick={this.onClickViewRow} />
             <Icons.VerticalSplitIcon onClick={this.onClickViewColumn} />
           </div>;
  }

  /// Render Preview using iframe
  renderPreview(location) {
    let url = `${Api.remote}/${location}`;
    return <div id="PreviewArea">
             {this.renderHeader(location)}
             <iframe className="previewIframe"
                     src={url}
                     title="preview"
                     ref={r => {
                       this.iframe = r;
                       this.iframe.onload = this.onLoad;
                     }}/>
           </div>;
  }

  render() {
    if (this.props.location) {
      return this.renderPreview(this.props.location);
    } else {
      return this.renderLoading();
    }
  }
}


PreviewArea.propTypes = {
  location: PropTypes.string
}

export default PreviewArea;
