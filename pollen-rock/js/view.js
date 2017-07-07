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
    this.editorPostInitHandler = this.makeEditorReady.bind(this);
    this.statusChangeHandler = this.changeSaveStatus.bind(this);
    return this;
  }

  attach() {
    this.model.editorInitFailEvent.attach(this.editorPostInitHandler);
    this.model.pollenTagsReadyEvent.attach(this.editorPostInitHandler);
    this.model.saveStatusChangeEvent.attach(this.statusChangeHandler);
    return this;
  }

  changeSaveStatus(sender, val) {
    if (! val)
      val = '';
    this.$saveStatus.text(val);
  }

  makeEditorReady() {
    this.$loader.addClass("hide");
    this.$editor.removeClass("hide");
  }

}
