import Axios from 'axios';

/// Send all GET/POST to this endpoint during development
const devRemote = "http://localhost:8000";

/// Send to this endpoint when in pord
const prodRemote = "";

const remote = devRemote;

function get(urlStr) {
  return Axios({
    method: 'get',
    url: urlStr
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
