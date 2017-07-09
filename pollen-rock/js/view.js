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
    // notify with an object whose properties are setting name,
    // property values are settings value
    this.editorSettingsChangeEvent = new Event(this);

    return this;
  }

  setupHandlers() {
    this.editorPostInitHandler = this.editorPostInit.bind(this);
    this.statusChangeHandler = this.changeSaveStatus.bind(this);

    this.previewBtnHandler = () => { this.previewRequestEvent.notify(); };
    this.previewReadyHandler = this.previewReady.bind(this);

    // settings handlers
    this.editorSettingsChangeHandler = this.editorSettingsChange.bind(this);
    this.keymapChangeHandler = this.buildKeymaps.bind(this);

    return this;
  }

  attach() {
    this.model.editorInitFailEvent.attach(this.editorPostInitHandler);
    this.model.pollenTagsReadyEvent.attach(this.editorPostInitHandler);
    this.model.saveStatusChangeEvent.attach(this.statusChangeHandler);
    this.model.previewReadyEvent.attach(this.previewReadyHandler);
    this.model.keymapChangeEvent.attach(this.keymapChangeHandler);
    this.model.editorSettingsChangeEvent.attach(this.editorSettingsChangeHandler);

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
    let $tableBody = $("#keymap-body");
    $tableBody.html('');

    let rowTemplate = $('script[data-template="keymap-row"]').html();
    // iterate through all the keys
    console.log(`keymap: ${maps}`);
    for (let [k, v] of maps) {
      let row = rowTemplate
          .replace(/{{keystroke}}/g, k)
          .replace(/{{doc}}/g, v.doc);
      $tableBody.append(row);
    }
  }

  /**
   * received new settings from model, update preference UI
   */
  editorSettingsChange(sender, settings) {
    let $preference = $("#preference");
    let self = this;
    $preference.html('');

    for (let name of settings.keys()) {
      // create UI
      let $inputDiv = $('<div>').addClass('input-field col s12');
      let $select = $('<select>').attr('id', `p-${name}`).appendTo($inputDiv);
      for (let option of settings.options(name)) {
        let $option = $('<option>').attr('value', option).text(option);
        $select.append($option);
      }
      let $label = $('<label>').text(name).appendTo($inputDiv);
      // setup bindings
      $select.on("change", ((nm) => function(e) {
        let changed = {};
        changed[nm] = $(this).val();
        self.editorSettingsChangeEvent.notify(changed);
        $('select').material_select();
      })(name));
      $preference.append($inputDiv);
    }
    $('select').material_select();
  }
}
