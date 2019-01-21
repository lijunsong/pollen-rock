import React, { Component } from 'react';
import * as Api from './Api';
import * as Icons from './Icons';
import * as Notify from './Notify';
import PropTypes from 'prop-types';


class PreviewArea extends Component {
  constructor(props) {
    super(props);
    this.state = {
      location: null,
      loading: true,
    };
    this.onClickViewColumn = () => this.props.onClickDirection("vertical");
    this.onClickViewRow = () => this.props.onClickDirection("horizontal");
    this.onClickRefresh = this.onClickRefresh.bind(this);
    this.onLoad = this.onLoad.bind(this);
  }

  onLoad() {
    if (this.iframe) {
      console.log("Install select handler in iframe");
      let contentWindow = this.iframe.contentWindow;
      contentWindow.addEventListener("mouseup", event => {
        let selected = contentWindow.getSelection().toString();
        // contract here is that false means no selection
        this.props.onTextSelected(selected || false);
      });
    }
    this.setState({loading: false});
  }

  onClickRefresh() {
    if (this.iframe) {
      this.setState({loading: true});
      let location = this.props.location;
      Api.render(location).then(() => {
        this.iframe.contentWindow.location.reload(true);
      }).catch((e) => {
        Notify.error(`Failed to refresh ${location}`);
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
      this.setState({loading: true});
      this.iframe.contentWindow.location.reload(true);
    } catch (err) {
      console.warn("Failed to reload, likely caused by same-origin policy");
      this.iframe.src += '';
    }
  }

  async fetchLocation(path) {
    this.setState({loading: true});

    let res = await Api.render(path);

    // set the location anyway because the server always returns
    // location to us
    this.setState({location: res.data.location});

    if (res.data.errno !== 0) {
      console.error(`Render failed on ${path}`);
    }
  }

  componentDidUpdate(prevProps) {
    if (prevProps.path !== this.props.path) {
      this.setState({location: null});
      this.fetchLocation(this.props.path);
    }

    if (this.iframe) {
      this.iframe.onload = this.onLoad;
    }
  }

  componentDidMount() {
    this.fetchLocation(this.props.path);
  }

  renderHeader(location) {
    if (! location) {
      location = "Loading...";
    }
    let refreshClass = this.state.loading ? "spin" : "";
    return <div id="PreviewHeader">
             <span id="PreviewPath">{location}</span>
             <Icons.IconRefresh className={`clickable ${refreshClass}`}
                                onClick={this.onClickRefresh} />
             <Icons.IconHSplit className="clickable"
                               onClick={this.onClickViewRow} />
             <Icons.IconVSplit className="clickable"
                               onClick={this.onClickViewColumn} />
           </div>;
  }

  /// Render Preview using iframe
  renderIframe() {
    if (! this.state.location) {
      return <div className="Loading"></div>;
    }

    let url = `${Api.remote}/${this.state.location}`;
    // add previewIframe div and set overflow-scrolling touch there
    // to let small device scroll the iframe
    return <div className="previewIframe">
               <iframe className="preview"
                       src={url}
                       title="preview"
                       ref={r => this.iframe = r } />
           </div>;
  }

  render() {
    return <div id="PreviewArea">
             {this.renderHeader(this.state.location)}
             {this.renderIframe()}
           </div>;
  }
}


PreviewArea.propTypes = {
  path: PropTypes.string.isRequired,
  onTextSelected: PropTypes.func.isRequired,
  onClickDirection: PropTypes.func.isRequired,
};

export default PreviewArea;
