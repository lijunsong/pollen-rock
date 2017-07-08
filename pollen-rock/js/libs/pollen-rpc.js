// JsonRPC object is an object that has at least three properties:
// id, method, result. RPCResult is an object that has at least three
// properties: id, result, error.

class RPCVal {
  constructor(id, result, error) {
    this.id = id;
    this.result = result;
    this.error = error;
    if (this.result == null && this.error == null) {
      throw "Not RPCVal";
    }
  }
}

class RPCResultVal extends RPCVal {
  constructor(id, result) {
    super(id, result, null);
  }
}

class RPCErrorVal extends RPCVal {
  constructor(id, error) {
    super(id, null, error);
  }
}

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
          resolve(new RPCResultVal(res.id, res.result));
        } else {
          reject(new RPCResultVal(res.id, res.error));
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
