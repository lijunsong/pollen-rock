import Axios from 'axios';

/// DO NOT import process again. Experiment shows it will overwrite
/// data in process.env
export const remote = process.env.REACT_APP_DEV_REMOTE || "";

function get(urlStr) {
  return Axios({
    method: 'GET',
    url: urlStr,
  });
}

function post(urlStr, data) {
  let str = [];
  for (var key in data) {
    str.push(encodeURIComponent(key) + '=' + encodeURIComponent(data[key]));
  }
  let encodedData = str.join('&');

  return Axios({
    method: 'POST',
    url: urlStr,
    headers: { 'content-type': 'application/x-www-form-urlencoded' },
    data: encodedData,
  });
}


export function getContents(path) {
  const url = `${remote}/rest/fs/${path}`;
  return get(url);
}

export function getConfig(path) {
  path = path || "";
  const url = `${remote}/rest/config/${path}`;
  return get(url);
}

export function saveContents(path, contents) {
  const url = `${remote}/rest/fs/${path}`;
  const postData = {
    op: "write",
    data: contents,
  };
  return post(url, postData);
}

/// render the path. The path can be destination or source
export function render(path) {
  const url = `${remote}/rest/render/${path}`;
  return get(url);
}

/// Long polling the path
export function watch(path) {
  const url = `${remote}/rest/watch/${path}`;
  return Axios({
    method: 'GET',
    url: url,
    timeout: 0,
  });
}
