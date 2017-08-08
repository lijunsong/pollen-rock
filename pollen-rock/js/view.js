"use strict";

/**
 * View is responsible for all the DOMs. View sees UI events like
 * clicking the changing preference.
 *
 * View subscribes to Model's events directly so it can respond to
 * Model's change directly. For example, Model notifies
 * autorenderReadyEvent after it asks server to render autorender. View
 * subscribes to that event, and load the autorender immediately when
 * autorender is available. Note that View's events will not be
 * subscribed by Model. Bidirectional Events will only complicate the
 * whole thing.
 *
 *
 */

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

    this.$autorender = $("#autorender-wrapper");
    this.$autorenderFrame = this.$autorender.find('#autorender-frame');
    this.$autorenderLoader = this.$autorender.find("#autorender-loader");
    this.$autorenderBtn = $("#autorenderBtn");
    this.$backBtn = $("#backBtn");

    /* ---------- events ---------- */
    this.editorPositionChangeEvent = new Event(this);
    // notify when autorender is clicked
    this.autorenderRequestEvent = new Event(this);
    // notify with a new setting when settings are changed in UI
    this.editorSettingsChangeEvent = new Event(this);
    // notify when fullscreen is triggered in UI
    this.fullscreenEvent = new Event(this);

    return this;
  }

  setupHandlers() {
    this.editorPostInitHandler = this.editorPostInit.bind(this);
    this.statusChangeHandler = this.changeSaveStatus.bind(this);

    this.autorenderBtnHandler = () => { this.autorenderRequestEvent.notify(); };
    this.autorenderReadyHandler = this.autorenderReady.bind(this);

    this.backBtnHandler = () => window.history.back();

    this.editorSettingsChangeHandler = this.editorSettingsChange.bind(this);
    this.keymapChangeHandler = this.buildKeymaps.bind(this);

    this.fullscreenHandler = this.searchFullScreenFunction();

    return this;
  }

  attach() {
    // attach to model's event
    this.model.pollenSetupReadyEvent.attach(this.editorPostInitHandler);
    this.model.saveStatusChangeEvent.attach(this.statusChangeHandler);
    this.model.autorenderReadyEvent.attach(this.autorenderReadyHandler);
    this.model.keymapChangeEvent.attach(this.keymapChangeHandler);
    this.model.editorSettingsChangeEvent.attach(this.editorSettingsChangeHandler);

    this.fullscreenEvent.attach(this.fullscreenHandler);

    this.$autorenderBtn.click(this.autorenderBtnHandler);
    this.$backBtn.click(this.backBtnHandler);

    return this;
  }

  setupEditorPreference(settings) {
    let $preference = $("#preference");
    let self = this;
    $preference.html('');

    // handlers for selecting new value. These DOMs haven't been
    // attached to the tree yet, and we can't even grab these DOMs in
    // the following init steps, so let's just setup callbacks in this
    // method
    let callback = (name) => function() {
      let change = {},
          val = $(this).val();
      if (val === "true") val = true;
      else if (val === "false") val = false;
      else if (/^\d+$/.test(val)) val = Number(val);
      change[name] = val;
      self.editorSettingsChangeEvent.notify(change);
    };


    for (let name of settings.keys()) {
      let val = settings.value(name);
      // create UI
      let $inputDiv = $('<div>').addClass('input-field col s12');

      if (settings.options(name)) {
        let $select = $('<select>').attr('id', `p-${name}`).appendTo($inputDiv);
        for (let option of settings.options(name)) {
          let $option = $('<option>').attr('value', option+'').text(option+'');
          if (option === val) {
            $option.attr('selected', '');
          }
          $select.append($option);
        }
        let $label = $('<label>').text(name).appendTo($inputDiv);
        $select.on("change", callback(name));
      } else {
        let id = `p-${name}`;
        let $input = $('<input>').attr('id', id).attr('type', 'text')
            .attr('value', val).appendTo($inputDiv);
        let $label = $('<label>').attr('for', id).text(name).appendTo($inputDiv);
        $input.on("input", callback(name));
      }
      $preference.append($inputDiv);
    }
    $('select').material_select();
    Materialize.updateTextFields();
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

  autorenderReady(sender, path) {
    if (this.$autorenderFrame.attr('src')) {
      let doc = this.$autorenderFrame.contents()[0];
      doc.location.reload(true);
    } else {
      this.$autorenderFrame.attr('src', path);
    }
    this.$autorenderLoader.addClass("hide");
  }

  editorPostInit() {
    this.$startupLoader.addClass("hide");
    this.$editor.removeClass("hide");
    this.editorPositionChangeEvent.notify();
    notifyInfo("Ready to Rock!");
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

  showAutorenderLoader() {
    this.$autorenderLoader.removeClass("hide");
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

    if (settings.hasOwnProperty('font')) {
      $(".CodeMirror").css('font-family', settings['font']);
    }
  }
}
