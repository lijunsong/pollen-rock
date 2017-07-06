

class View {
  constructor(model) {
    this.model = model;
    this.setupBindings()
      .setupHandlers()
      .enable();
  }

  setupBindings() {
    this.$editor = $("#editor-wrapper");
    this.$saveStatus = $("#save-status");
    this.$loader = $("#loader");

    return this;
  }

  setupHandlers() {
    this.initCodeMirrorHandler = this.initCodeMirror.bind(this);
    this.saveStatusChangehandler = this.saveStatusChange.bind(this);
    return this;
  }

  enable() {
    this.model.fetchPollenConfigEvent.attach(this.initCodeMirrorHandler);
    this.model.fetchPollenConfigEvent.attach
    this.model.saveStatusChangeEvent.attach(this.saveStatusChangeHandler);
    return this;
  }

  /* ---------- CodeMirror setups ---------- */

  initCodeMirror(config) {
    this.editor = CodeMirror.fromTextArea($("#compose"), config);
    this.initCodeMirrorKeyMaps();
  }

  initCodeMirrorKeyMaps() {
  }

  /* ---------- CodeMirror setup ends ---------- */

  saveStatusChange(val) {
    this.$saveStatus.text(val);
  }

  

}
