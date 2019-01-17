import CodeMirror from 'codemirror';
import 'codemirror/mode/meta';
import React, { Component } from 'react';
import PropTypes from 'prop-types';
import 'codemirror/lib/codemirror.css';

export default class ReactCodeMirror extends Component {
  constructor(props) {
    super(props);
    this.editor = null;
  }

  async componentDidMount() {
    let {events, options, height, width, value} = this.props;

    this.editor = CodeMirror.fromTextArea(this.textarea, options);
    for (const [name, e] of Object.entries(events)) {
      this.editor.on(name, e);
    };

    this.editor.setValue(value || '');
    this.editor.setSize(width, height);

    const mode = CodeMirror.findModeByName(options.mode);
    if (mode && mode.mode) {
      await import(`codemirror/mode/${mode.mode}/${mode.mode}.js`);
    }
    if (mode) {
      options.mode = mode.mime;
    }
    for (const [k, v] of Object.entries(options)) {
      this.editor.setOption(k, v);
    }
    this.editor.refresh();
  }

  componentWillUnmount() {
    if (this.editor) {
      this.editor.toTextArea();
    }
  }

  render() {
    return <textarea ref={r => this.textarea = r} />;
  }
}

ReactCodeMirror.defaultProps = {
  value: '',
  options: {},
  events: {},
  width: '100%',
  height: '100%',
};

ReactCodeMirror.propTypes = {
  value: PropTypes.string,
  options: PropTypes.object,
  events: PropTypes.object,
  width: PropTypes.oneOfType([PropTypes.string, PropTypes.number]),
  height: PropTypes.oneOfType([PropTypes.string, PropTypes.number]),
};
