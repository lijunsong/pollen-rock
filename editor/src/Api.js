import Axios from 'axios';

/// Send all GET/POST to this endpoint during development
const devRemote = "http://localhost:8000";

/// Send to this endpoint when in pord
const prodRemote = "";

export const remote = devRemote;


function get(urlStr) {
  return Axios({
    method: 'GET',
    url: urlStr
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

export function render(path) {
  const url = `${remote}/rest/render/${path}`;
  return get(url);
}
