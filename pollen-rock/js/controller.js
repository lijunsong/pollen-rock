"use strict";

// Rule of thumbs on Event subscription: 1. View directly subscribes
// to model's event.  2. View's events  to which model should
// response will be set up by Controller

class Controller {
  constructor(model, view) {
    this.model = model;
    this.view = view;

    this.setupHandlers()
      .attach();

    this.init();
  }

  setupHandlers() {
    this.previewHandler = this.preview.bind(this);
    this.editorPositionChangeHandler = this.editorPositionChange.bind(this);
    this.editorSettingsChangeHandler = this.editorSettingsChange.bind(this);
    return this;
  }

  attach() {
    this.view.previewRequestEvent.attach(this.previewHandler);
    this.view.editorPositionChangeEvent.attach(this.editorPositionChangeHandler);
    this.view.editorSettingsChangeEvent.attach(this.editorSettingsChangeHandler);
    return this;
  }

  init() {
    this.model.init();
    this.model.addKeyMap(this.getKeyMaps());
  }

  /* ---------- keymap handlers ---------- */
  insertCommandCharHandler(e) {
    let pos = e.getCursor();
    let commandChar = this.model.getPollenSetup('command-char');
    if (pos.ch == 0) {
      e.replaceSelection(commandChar);
    } else {
      let lastPos = CodeMirror.Pos(pos.line, pos.ch-1);
      if (e.getRange(lastPos, pos) == commandChar) {
        e.replaceRange('@', lastPos, pos);
      } else {
        e.replaceSelection(commandChar);
      }
    }
  }

  /* ---------- keymap handlers ends ---------- */
  getKeyMaps() {
    return [
      new Keymap('Shift-2',
        this.insertCommandCharHandler.bind(this),
        "Insert Command Char or @"
      )
    ];
  }

  editorPositionChange() {
    this.model.refreshEditor();
  }

  /**
   * subscribe to view's settings change
   */
  editorSettingsChange(sender, settings) {
    this.model.changeEditorSettings(settings);
  }

  preview() {
    let $preview = this.view.$preview;
    $preview.toggleClass("hide");
    if ($preview.hasClass("hide")) {
      this.view.placeEditorOnCenter();
    } else {
      this.view.showPreviewLoader();
      this.model.renderPreview();
      this.view.placeEditorOnRight();
    }
  }
}
