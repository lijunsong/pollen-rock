import React from 'react';
import ReactDOM from 'react-dom';
import axios from 'axios';
import ReactCodeMirror from './ReactCodeMirror';
import {EditorHeader, EditorBody, EditorFooter} from './Editor';
import { shallow, mount, render } from 'enzyme';
import Adapter from 'enzyme-adapter-react-16';
import Enzyme from 'enzyme';
import 'codemirror/mode/scheme/scheme';


// for the rendered output, you can do console.log(wrapper.debug());
Enzyme.configure({ adapter: new Adapter() });
// mock axios
jest.mock('axios');

/// Inject necessary method so codemirror can run successfully
global.navigator = 'gecko';

document.body.createTextRange = function() {
  return {
    setEnd: function(){},
    setStart: function(){},
    getClientRects: function(){
      return {right: 0};
    },
    getBoundingClientRect: function(){
      return {right: 0};
    }
  };
};

const FILE_PATH = "/test.html.pm";
const FILE_GET_RESPONSE = {errno: 0, contents: `#lang racket
◊section{section data}
`};
const FILE_CONFIG_RESPONSE = {errno: 0, tags: [{
  "kind": "variable",
  "name": "x",
  "type": "number",
  "value": 1
}]};

axios.get.mockImplementation((url) => {
  if (url.startsWith("/rest/fs") && ! url.endsWith("/")) {
    return Promise.resolve({
      data: FILE_GET_RESPONSE,
    });
  } else if (url.startsWith("/rest/config") || url.startsWith("/rest/tags")) {
    return Promise.resolve({
      data: FILE_CONFIG_RESPONSE,
    });
  }
  throw new Error("URL mock failed: " + url);
});

it('EditorBody renders', async() => {
  const flushPromise = () =>
        new Promise(resolve => window.setTimeout(resolve, 1000));

  let comp = <EditorBody path={"/test.html.pm"}
                         onSyntaxCheck={()=>{}}
                         onCursorActivity={()=>{}}/>;

  const wrapper = mount(comp);
  await flushPromise();
  wrapper.update();

  // Check the tags state: it should be a map (obejct) instead of a list
  expect(wrapper.state('tags').x.value).toEqual(1);

  let reactCM = wrapper.find(ReactCodeMirror);
  // ReactCodeMirror is mounted
  expect(reactCM.exists()).toBe(true);
  // the mode should be pollen
  expect(reactCM.props().options.mode).toEqual("pollen");
  // the cursorActivity props should be passed in as well
  expect(reactCM.props().events.cursorActivity).toBeDefined();

  wrapper.unmount();
});

it('ReactCodeMirror gets no cursorActivity for non-pollen mode', async() => {
  const flushPromise = () =>
        new Promise(resolve => window.setTimeout(resolve, 1000));

  /// NOTE: I used to use react-codemirror package, which calls await
  /// import on mode lazy load. The import looks like not compatible
  /// here (test says Not supported), so I wrote a simple
  /// ReactCodeMirror component. It's actually quite simple.
  let comp = <EditorBody path={"/test.ss"}
                         onSyntaxCheck={()=>{}}
                         onCursorActivity={()=>{}}/>;

  const wrapper = mount(comp);
  await flushPromise();
  wrapper.update();

  let reactCM = wrapper.find(ReactCodeMirror);
  // ReactCodeMirror is mounted
  expect(reactCM.exists()).toBe(true);
  // the mode should be pollen
  expect(reactCM.props().options.mode).not.toEqual("pollen");
  // the cursorActivity props should not be passed in
  expect(reactCM.props().events.cursorActivity).toBeFalsy();
  wrapper.unmount();
});
