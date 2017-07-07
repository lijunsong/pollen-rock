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
    return this;
  }

  attach() {
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
}
