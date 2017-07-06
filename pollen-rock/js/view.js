class View {
  constructor(model) {
    this.model = model;
    this.setupBindings()
      .setupHandlers()
      .attach();
  }

  setupBindings() {
    this.$editor = $("#editor-wrapper");
    this.$saveStatus = $("#save-status");
    this.$loader = $("#loader");

    return this;
  }

  setupHandlers() {
    this.saveStatusChangehandler = this.changeSaveStatus.bind(this);
    this.editorPostInitHandler = this.makeEditorReady.bind(this);
    return this;
  }

  attach() {
    this.model.editorInitFailEvent.attach(this.editorPostInitHandler);
    this.model.editorTagsReadyEvent.attach(this.editorPostInitHandler);
    this.model.saveStatusChangeEvent.attach(this.saveStatusChangeHandler);
    return this;
  }

  changeSaveStatus(val) {
    this.$saveStatus.text(val);
  }

  makeEditorReady() {
    this.$loader.addClass("hide");
    this.$editor.removeClass("hide");
  }

}
