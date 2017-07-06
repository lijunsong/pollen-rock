function notifyInfo(msg) {
  Materialize.toast(msg, 4000, 'toast-info');
}


function notifyError(msg) {
  Materialize.toast(msg, 4000, 'toast-error');
}

class Model {
  constructor(resource) {
    this.rpc = new PollenRockRPC("/api");

    this.projectConfig = {};
    this.tags = {};
    this.editorPreference = {};

    this.fetchConfigFailEvent = new Event(this);
    this.fetchTagsSuccessEvent = new Event(this);
    this.fetchProjectConfigSuccessEvent = new Event(this);
    this.saveStatusChangeEvent = new Event(this);

    this.resource = resource;
  }

  fetchPollenConfig() {
    return this.rpc.call_server("get-project-config", this.resource).then(v => {
      let config = v.result;
      this.fetchTagsSuccessEvent.notify(config['tags']);
      this.fetchProjectConfigSuccessEvent.notify(config['projectConfig']);
    }).catch(err => {
      this.fetchConfigFailEvent.notify(err);
    });
  }
}
