// RPC caller object is an object that has at least three properties:
// id, method, result. RPC result is an object that has at least three
// properties: id, result, error.

// JsonRPC
class JsonRPC {
  constructor(server_url) {
    this.id = 0;
    this.server_url = server_url;
  }

  // string -> string -> Promise
  _rpc_call(method, params_string) {
    let self = this;
    return new Promise((resolve, reject) => {
      self.id += 1;
      let data = {
        id: self.id,  // not used for now
        method: method,
        params: params_string
      };

      $.post(self.server_url, data, function(result) {
        let res = JSON.parse(result);
        if (res.result) {
          resolve(res);
        } else {
          reject(res);
        }
      });
    });
  }
}

class PollenRockRPC extends JsonRPC {
  // call_server takes a remote procedure name and variable number of
  // arguments, and returns a promise that resolves to a RPC result.
  call_server(method, ... params) {
    return this._rpc_call(method, JSON.stringify(params));
  }
}
