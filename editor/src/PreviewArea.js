import React, { Component } from 'react';
import * as Api from './Api';

class PreviewArea extends Component {
  constructor(props) {
    super(props);
    this.state = {
      location: null,
    };
  }

  async renderRequest(path) {
    let res = await Api.render(path);
    if (res.data.errno !== 0) {
      throw new Error(`Render failed on ${path}`);
    }

    this.setState({location: res.data.location});
  }

  async watchPath(path) {
    console.log("Watching " + path);
    let res = await Api.watch(path);
    if (res.data.errno !== 0) {
      throw new Error(`This file is removed`);
    }

    this.reloadIframe();

    return this.watchPath(path);
  }

  reloadIframe() {
    if (! this.iframe) {
      return;
    }

    console.log("reloading iframe");
    try {
      this.iframe.contentWindow.location.reload(true);
      this.installSelectHandler(this.iframe.contentWindow);
    } catch (err) {
      console.warn("Please switch to Production build to use the preview feature");
      console.warn("Because of same-origin policy, the editor can only refresh blindly");
      this.iframe.src += '';
    }
  }

  installSelectHandler(contentWindow) {
    contentWindow.addEventListener("mouseup", event => {
      let selected = contentWindow.getSelection().toString();
      if (selected) {
        this.props.onSelected(selected);
      } else {
        this.props.onSelected(false);
      }
    });
  }

  componentDidMount() {
    let path = this.props.path;

    if (!path) {
      return;
    }

    this.renderRequest(path).then(() => {
      console.log(`Successfully render ${path}`);
      this.reloadIframe();
    }).catch((e) => {
      console.log(`Failed to render ${path}: ${e}`);
    });

    this.watchPath(path);
  }

  renderLoading() {
    return <div id="PreviewArea">
             "Loading";
           </div>;
  }

  renderPreview(location) {
    let url = `${Api.remote}/${this.state.location}`;
    return <div id="PreviewArea">
             <iframe src={url}
                     ref={r => this.iframe=r}/>
           </div>;
  }

  render() {
    console.log("rendering preview");
    if (this.state.location) {
      return this.renderPreview(this.state.location);
    } else {
      return this.renderLoading();
    }
  }
}

export default PreviewArea;
