function notifyInfo(msg) {
  Materialize.toast(msg, 4000, 'toast-info');
}


function notifyError(msg) {
  Materialize.toast(msg, 4000, 'toast-error');
}

class Model {
  constructor() {
    this.rpc = new PollenRockRPC("/api");

    this.pollenConfig = {};
    this.pollenTags = {};
    this.editorPreference = {};

    this.fetchPollenConfigEvent = new Event(this);

    this.saveStatusChangeEvent = new Event(this);
  }

  fetchPollenConfig() {
    return this.rpc.call_server("get-project-config", resource).then(v => {
      let jsconfig = v.result;
      this.fetchPollenConfigEvent.notify(jsconfig);
    }).catch(err => {
      this.fetchPollenConfigEvent.notify(err);
    });
  }
}
