"use strict";

/**
 * View is responsible for all the DOMs. View sees UI events like
 * clicking the changing preference.
 *
 * View subscribes to Model's events directly so it can respond to
 * Model's change directly. For example, Model notifies
 * previewReadyEvent after it asks server to render preview. View
 * subscribes to that event, and load the preview immediately when
 * preview is available. Note that View's events will not be
 * subscribed by Model. Bidirectional Events will only complicate the
 * whole thing.
 *
 *
 */

class View {
  constructor(model) {
    this.model = model;
    this.setupMaterialize();

    this.setupEditorSettings();

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
    // notify when preview is clicked
    this.previewRequestEvent = new Event(this);
    // notify with a new setting when settings are changed in UI
    this.editorSettingsChangeEvent = new Event(this);
    // notify when fullscreen is triggered in UI
    this.fullscreenEvent = new Event(this);

    return this;
  }

  setupHandlers() {
    this.editorPostInitHandler = this.editorPostInit.bind(this);
    this.statusChangeHandler = this.changeSaveStatus.bind(this);

    this.previewBtnHandler = () => { this.previewRequestEvent.notify(); };
    this.previewReadyHandler = this.previewReady.bind(this);

    this.editorSettingsChangeHandler = this.editorSettingsChange.bind(this);
    this.keymapChangeHandler = this.buildKeymaps.bind(this);

    this.fullscreenHandler = this.searchFullScreenFunction();

    return this;
  }

  attach() {
    // attach to model's event
    this.model.editorInitFailEvent.attach(this.editorPostInitHandler);
    this.model.pollenTagsReadyEvent.attach(this.editorPostInitHandler);
    this.model.saveStatusChangeEvent.attach(this.statusChangeHandler);
    this.model.previewReadyEvent.attach(this.previewReadyHandler);
    this.model.keymapChangeEvent.attach(this.keymapChangeHandler);
    this.model.editorSettingsChangeEvent.attach(this.editorSettingsChangeHandler);

    this.$previewBtn.click(this.previewBtnHandler);
    this.fullscreenEvent.attach(this.fullscreenHandler);

    return this;
  }

  setupEditorSettings() {
    let settings = this.model.editorSettings;
    let $preference = $("#preference");
    let self = this;
    $preference.html('');

    // handlers for selecting new value. These DOMs haven't been
    // attached to the tree yet, and we can't even grab these DOMs in
    // the following init steps, so let's just setup callbacks in this
    // method
    let callback = (name) => function() {
      let change = {};
      change[name] = $(this).val();
      self.editorSettingsChangeEvent.notify(change);
    };


    for (let name of settings.keys()) {
      // create UI
      let $inputDiv = $('<div>').addClass('input-field col s12');
      let $select = $('<select>').attr('id', `p-${name}`).appendTo($inputDiv);
      for (let option of settings.options(name)) {
        let $option = $('<option>').attr('value', option).text(option);
        $select.append($option);
      }
      let $label = $('<label>').text(name).appendTo($inputDiv);
      $preference.append($inputDiv);
      $select.on("change", callback(name));
    }
    $('select').material_select();
    return this;
  }

  searchFullScreenFunction() {
    let lst = ["requestFullscreen", "mozRequestFullScreen",
             "webkitRequestFullscreen", "msRequestFullscreen"];
    for (let f of lst) {
      if (f in document.documentElement) {
        return document.documentElement[f].bind(document.documentElement);
      }
    }
    return () => notifyError("Your browser doesn't support fullscreen");
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
    for (let [k, v] of maps) {
      let row = rowTemplate
          .replace(/{{keystroke}}/g, k)
          .replace(/{{doc}}/g, v.doc);
      $tableBody.append(row);
    }
  }

  editorSettingsChange(sender, settings) {
    // we don't need to do anything about the change in model's
    // settings, except that we need to update background if editor
    // theme is changed: we need to update general background to match
    // the editor's
    if (settings.hasOwnProperty('theme')) {
      $("#upper-wrapper").css(
        'background-color',
        $(".CodeMirror-wrap").css('background-color'));
    }
  }
}
