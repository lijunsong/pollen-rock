import React, { Component } from 'react';
import * as Api from './Api';
import * as Icons from './Icons';
import { notify } from './Notify';
import PropTypes from 'prop-types';


class PreviewArea extends Component {
  constructor(props) {
    super(props);
    this.onClickViewColumn = () => this.props.onClickDirection("vertical");
    this.onClickViewRow = () => this.props.onClickDirection("horizontal");
    this.onClickRefresh = this.onClickRefresh.bind(this);
    this.onLoad = this.onLoad.bind(this);
  }

  onLoad() {
    if (this.iframe) {
      this.installSelectHandler(this.iframe.contentWindow);
    }
  }

  onClickRefresh() {
    if (this.iframe) {
      let location = this.props.location;
      Api.render(location).then(() => {
        this.iframe.contentWindow.location.reload(true);
      }).catch((e) => {
        notify.error(`Failed to refresh ${location}`);
      });
    }
  }

  /// reload will reload the iframe (this function is provided for parent
  /// component)
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

  componentDidUpdate(prevProps) {
    if (this.iframe) {
      this.iframe.onload = this.onLoad;
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
             <Icons.IconRefresh className="clickable"
                                onClick={this.onClickRefresh} />
             <Icons.IconHSplit className="clickable"
                               onClick={this.onClickViewRow} />
             <Icons.IconVSplit className="clickable"
                               onClick={this.onClickViewColumn} />
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
                     ref={r => this.iframe = r } />
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
  location: PropTypes.string,
  onTextSelected: PropTypes.func.isRequired,
  onClickDirection: PropTypes.func.isRequired,
};

export default PreviewArea;
