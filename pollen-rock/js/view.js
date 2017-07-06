

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
    this.fetchConfigHandler = this.fetchConfig.bind(this);
    return this;
  }

  enable() {
    this.model.fetchProjectConfigSuccessEvent.attach(this.initCodeMirrorHandler);
    this.model.fetchProjectConfigSuccessEvent.attach(this.fetchConfigHandler);
    this.model.fetchConfigFailEvent.attach(this.fetchConfigHandler);

    this.model.saveStatusChangeEvent.attach(this.saveStatusChangeHandler);
    return this;
  }

  /* ---------- CodeMirror setups ---------- */

  initCodeMirror() {
    let config = {
      autofocus: true,
      matchBrackets: true,
      lineWrapping: true,
      scrollbarStyle: "null",
      theme: "default",
      mode: 'pollen'
    };
    this.editor = CodeMirror.fromTextArea(
      document.getElementById('compose'),
      config
    );
    this.initCodeMirrorKeyMaps();
  }

  initCodeMirrorKeyMaps() {
  }

  /* ---------- CodeMirror setup ends ---------- */

  saveStatusChange(val) {
    this.$saveStatus.text(val);
  }

  fetchConfig() {
    this.$loader.addClass("hide");
    this.$editor.removeClass("hide");
    if (this.editor)
      this.editor.refresh();
  }

}
