import React from 'react';
import ReactDOM from 'react-dom';
import axios from 'axios';
import App from './App';
import { shallow, mount, render } from 'enzyme';
import Adapter from 'enzyme-adapter-react-16';
import Enzyme from 'enzyme';


// for the rendered output, you can do console.log(wrapper.debug());
Enzyme.configure({ adapter: new Adapter() });
// mock axios
jest.mock('axios');

it('render entry list', async () => {
  const flushPromises = () =>
        new Promise(resolve => window.setTimeout(resolve, 1000));

  const ret = {
    errno: 0,
    items: [
      "compiled/",
      "dir1/",
      "file1.html.pm",
      "file2.html.pm",
      "pollen.rkt"
    ]
  };

  axios.get.mockResolvedValue({data: ret});

  const wrapper = mount(<App />);
  expect(wrapper.exists("#Splash")).toBe(true);
  let entryWrapper = wrapper.find("#NavEntries");
  expect(entryWrapper.exists()).toBe(true);

  // let axios be fullfilled
  await flushPromises();

  // re-render it and check if we have entries
  wrapper.update();

  entryWrapper = wrapper.find("#NavEntries");
  expect(entryWrapper.find(".isFile").length).toBe(3);
  expect(entryWrapper.find(".isDir").length).toBe(2);
});
