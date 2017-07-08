"use strict";

class View {
  constructor(model) {
    this.model = model;
    this.setupMaterialize();
    this.setupBindings()
      .setupHandlers()
      .attach();
  }

  setupMaterialize() {
    $(".modal").modal({
      ready: function() {
        // Because we hide this tab before, to show this modal the first place
        // won't show tab's active indicator. I found one click would solve
        // this problem. So manually trigger a click event here.
        $("#tab-header li:first-child a").click();
      }
    });
    $('select').material_select();
  }

  setupBindings() {
    this.$editor = $("#editor-wrapper");
    this.$saveStatus = $("#save-status");
    this.$startupLoader = $("#startup-loader");

    this.$preview = $("#preview-wrapper");
    this.$previewFrame = this.$preview.find('#preview-frame');
    this.$previewLoader = this.$preview.find("#preview-loader");
    this.$previewBtn = $("#previewBtn");

    /* ---------- events ---------- */
    this.editorPositionChangeEvent = new Event(this);
    this.previewRequestEvent = new Event(this);

    return this;
  }

  setupHandlers() {
    this.editorPostInitHandler = this.editorPostInit.bind(this);
    this.statusChangeHandler = this.changeSaveStatus.bind(this);

    this.previewBtnHandler = () => { this.previewRequestEvent.notify(); };
    this.previewReadyHandler = this.previewReady.bind(this);
    this.keymapUpdateHandler = this.buildKeymaps.bind(this);

    return this;
  }

  attach() {
    this.model.editorInitFailEvent.attach(this.editorPostInitHandler);
    this.model.pollenTagsReadyEvent.attach(this.editorPostInitHandler);
    this.model.saveStatusChangeEvent.attach(this.statusChangeHandler);
    this.model.previewReadyEvent.attach(this.previewReadyHandler);
    this.model.keymapUpdateEvent.attach(this.keymapUpdateHandler);

    this.$previewBtn.click(this.previewBtnHandler);
    return this;
  }

  changeSaveStatus(sender, val) {
    if (! val)
      val = '';
    this.$saveStatus.text(val);
  }

  previewReady(sender, path) {
    if (this.$previewFrame.attr('src')) {
      let doc = this.$previewFrame.contents()[0];
      doc.location.reload(true);
    } else {
      this.$previewFrame.attr('src', path);
    }
    this.$previewLoader.addClass("hide");
  }

  editorPostInit() {
    this.$startupLoader.addClass("hide");
    this.$editor.removeClass("hide");
    this.editorPositionChangeEvent.notify();
  }

  placeEditorOnRight() {
    this.$editor.removeClass("push-m3");
    this.$editor.addClass("push-m6");
    this.editorPositionChangeEvent.notify();
  }

  placeEditorOnCenter() {
    this.$editor.removeClass("push-m6");
    this.$editor.addClass("push-m3");
    this.editorPositionChangeEvent.notify();
  }

  showPreviewLoader() {
    this.$previewLoader.removeClass("hide");
  }

  buildKeymaps(sender, maps) {
    // discard current keymap view, and build it again.
    let tableBody = $("#keymap-body");
    tableBody.html('');

    let rowTemplate = $('script[data-template="keymap-row"]').html();
    let modal = $("#keymap-settings");
    // iterate through all the keys
    console.log(`keymap: ${maps}`);
    for (let [k, v] of maps) {
      let row = rowTemplate
          .replace(/{{keystroke}}/g, k)
          .replace(/{{doc}}/g, v.doc);
      tableBody.append(row);
    }
  }

}
