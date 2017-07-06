function notifyInfo(msg) {
  Materialize.toast(msg, 4000, 'toast-info');
}


function notifyError(msg) {
  Materialize.toast(msg, 4000, 'toast-error');
}

class Model {
  constructor($textarea, resource) {
    this.resource = resource;

    this.rpc = new PollenRockRPC("/api");
    this.projectConfig = {};
    this.tags = {};

    this.editor = CodeMirror.fromTextArea($textarea, {
      autofocus: true,
      matchBrackets: true,
      lineWrapping: true,
      scrollbarStyle: "null",
      theme: "default",
      mode: 'pollen'
    });
    this.docGeneration = null;
    this.editorPreference = {};

    // Events
    this.editorInitFailEvent = new Event(this);
    this.editorTagsReadyEvent = new Event(this);
    this.projectConfigReadyEvent = new Event(this);
    this.saveStatusChangeEvent = new Event(this);
    this.editorChangedEvent = new Event(this);

    // Handlers
    this.setupHandlers()
      .attach();
  }

  setupHandlers() {
    return this;
  }

  attach() {
    this.editor.on("change", this.editorChangeEvent.notify);

    return this;
  }

  fetchProjectConfig() {
    return this.rpc.call_server("get-project-config", this.resource).then(v => {
      let config = v.result;
      this.tags = config['tags'];
      this.editorTagsReadyEvent.notify(this.tags);
      this.projectConfig = config['projectConfig'];
      this.projectConfigReadyEvent.notify(this.projectConfig);
    }).catch(err => {
      this.editorInitFailEvent.notify(err);
    });
  }

  save() {
    return new Promise((resolve, reject) => {
      if (this.editor.isClean(this.docGeneration)) {
        resolve();
      }

      this.docGeneration = this.editor.changeGeneration();
      let resource = this.resource;
      let text = this.editor.getValue();
      return this.rpc.call_server("save", text, resource);
    });
  }
}
