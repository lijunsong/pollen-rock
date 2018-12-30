import React, { Component } from 'react';
import * as Api from './Api';

class PreviewArea extends Component {
  constructor(props) {
    super(props);
    this.state = {
      location: null,
    };
  }

  onIframeLoad() {
    if (this.iframe) {
      this.installSelectHandler(this.iframe.contentWindow);
    }
  }

  async renderRequest(path) {
    let res = await Api.render(path);
    if (res.data.errno !== 0) {
      throw new Error(`Render failed on ${path}`);
    }

    this.setState({location: res.data.location});
  }

  async watchPath(path) {
    if (path !== this.props.path) {
      return;
    }

    console.log("Watching " + path);
    let res;
    try {
      res = await Api.watch(path);
      if (res.data.errno !== 0) {
        throw new Error(`This file is removed`);
      }
      this.reloadIframe();
    } catch (err) {
    }

    // FIXME: this watchPath will loop forever even if path is different
    // TO reproduce, reduce the Api.watch timeout to 5seconds, and open
    // a few docs and see the log. We need to cancel async tasks in
    // componentWillUnmount
    this.watchPath(path);
  }

  reloadIframe() {
    if (! this.iframe) {
      return;
    }

    try {
      /// always install new selectHandler after reload
      this.iframe.onload = this.onIframeLoad.bind(this);
      this.iframe.contentWindow.location.reload(true);
    } catch (err) {
      console.warn("Please switch to Production build to use the preview feature");
      console.warn("Because of same-origin policy, the editor can only refresh blindly");
      this.iframe.src += '';
    }
  }

  installSelectHandler(contentWindow) {
    console.log("Install select handler in iframe");
    contentWindow.addEventListener("mouseup", event => {
      let selected = contentWindow.getSelection().toString();
      // contract here is that false means no selection
      this.props.onSelected(selected || false);
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
                     title="preview"
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
