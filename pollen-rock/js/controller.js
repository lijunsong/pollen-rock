"use strict";

/**
 * Rule of thumbs on Event subscription: 1. View directly subscribes
 * to model's event.  2. View's events to which model should respond
 * will be set up by Controller.
 *
 * Model won't subscribe to View's events. Controller is responsible
 * for passing notification from View to Model. So here is what would
 * typically happend:
 *
 * User select a new theme. View sees the change in settings and
 * passes it to Controller (Controller subscribes to
 * View.editorSettingsChangeEvent). In the handler, Controller passes
 * the change to Model, and Model updates CodeMirror's theme option,
 * and notifies the world the change is made (via
 * Model.editorSettingsChangeEvent). Because view subscribes to that
 * event, it updates the background of the top level canvas to match
 * CodeMirror's background.
 *
 * (Yes, both View and Model have their own editorSettingsChangeEvent)
 *
 * One event can trigger tons of other events. It might be not
 * intuitive to see which one triggers which one, but this fan-out
 * event dispatcher helps extend all kinds of features quickly in a
 * manageable way.
 */

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
    this.saveStatusChangeHandler = this.saveStatusChange.bind(this);
    return this;
  }

  attach() {
    this.model.saveStatusChangeEvent.attach(this.saveStatusChangeHandler);

    this.view.previewRequestEvent.attach(this.previewHandler);
    this.view.editorPositionChangeEvent.attach(this.editorPositionChangeHandler);
    this.view.editorSettingsChangeEvent.attach(this.editorSettingsChangeHandler);
    return this;
  }

  init() {
    this.model.init();
    this.model.addCMKeyMap(this.getKeyMaps());
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

  fullscreenHandler(e) {
    this.view.fullscreenEvent.notify();
  }

  getKeyMaps() {
    return [
      new Keymap('Shift-2', this.insertCommandCharHandler.bind(this),
                 "Insert Command Char or @"),
      new Keymap('Shift-Ctrl-Enter', this.fullscreenHandler.bind(this),
                 "Enter Fullscreen"),
      new Keymap('Shift-Ctrl-P', this.preview.bind(this),
                 "Open/Close Preview"),
      new Keymap('Ctrl-Space', "autocomplete",
                 "Autocomplete Tag Names From pollen.rkt"),
    ];
  }
  /* ---------- keymap handlers ends ---------- */

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

  /**
   * When the doc is saved and preview is opened, try to refresh the
   * preview when syntax is correct. Note that view also has a
   * saveStatusChange handler, but that one only updates the text of
   * save status view.
   */
  saveStatusChange(sender, status) {
    if (status == 'saved'
        && ! this.view.$preview.hasClass("hide")
        && this.model.syntaxCheck()) {
      this.model.renderPreview();
    }
  }
}
