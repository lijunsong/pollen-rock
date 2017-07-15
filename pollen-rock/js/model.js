"use strict";

/**
 * Model is responsible for changes of all the data including the
 * CodeMirror editor, editor preference, pollen project config/setup.
 *
 * The change of data will be sent to event's subscriber. Subscriber
 * can subscribe (use attach function) to events of interests.
 *
 * Model's events can be subscribed by both View and Controller. One
 * example is saveStatusChangeEvent. View subscribes this event to
 * update the status text box, and Controller subscribes this event to
 * update preview when preview window is opened.
 */

function notifyInfo(msg) {
  Materialize.toast(msg, 4000, 'toast-info');
}

function notifyError(msg) {
  console.error(msg);
  msg = msg.replace(/\n/g, '<br />');
  Materialize.toast(msg, 15000, 'toast-error');
}

var DEFAULT_EDITOR_SETTINGS = {
  addModeClass: true
};

class Model {
  constructor($textarea, resource) {
    this.resource = resource;

    this.rpc = new PollenRockRPC("/api");
    this.pollenSetup = {};
    this.pollenTags = {};

    this.editorSettings = new EditorSettingsModel();
    this.editor = CodeMirror.fromTextArea(
      $textarea,
      this.editorSettings.serialize());
    this.setupEditorHint(this.editor);
    this.docGeneration = null;
    this.keyMaps = new Map();

    /// Events
    // notify with an object containing changed key and value
    this.editorSettingsChangeEvent = new Event(this);
    // notify when init fail
    this.editorInitFailEvent = new Event(this);
    // notify with pollenTags object
    this.pollenTagsReadyEvent = new Event(this);
    this.pollenSetupReadyEvent = new Event(this);
    // notify with saveStatus string
    this.saveStatusChangeEvent = new Event(this);
    // notify with null
    this.previewReadyEvent = new Event(this);
    // notify with a new Map
    this.keymapChangeEvent = new Event(this);

    // Handlers
    this.setupHandlers()
      .attach();
  }

  setupHandlers() {
    this.autoSaveHandler = this.makeAutoSaveHandler();
    this.saveStatusChangeHandler = () => {
      this.saveStatusChangeEvent.notify();
    };

    return this;
  }

  attach() {
    this.editor.on("change", this.autoSaveHandler);
    this.editor.on("change", this.saveStatusChangeHandler);

    this.pollenTagsReadyEvent.attach((_, tags) => this.pollenTags = tags);
    this.pollenSetupReadyEvent.attach((_, setup) => this.pollenSetup = setup);

    return this;
  }

  init() {
    this.rpc.call_server("get-project-config", this.resource).then(v => {
      let config = v.result;
      this.pollenTagsReadyEvent.notify(config['tags']);
      this.pollenSetupReadyEvent.notify(config['setup']);
    }).catch(err => {
      this.editorInitFailEvent.notify(err);
    });

    this.editorSettingsChangeEvent.notify(this.editorSettings);
  }

  refreshEditor() {
    this.editor.refresh();
  }

  /**
   * add key maps to the editor
   */
  addCMKeyMap(keymap_list) {
    for (let km of keymap_list) {
      this.keyMaps.set(km.key, km);
    }
    let kms = {};
    for (let [k, v] of this.keyMaps) {
      kms[k] = v.func;
    }
    this.editor.setOption("extraKeys", kms);
    this.keymapChangeEvent.notify(this.keyMaps);
  }

  setupEditorHint(editor) {
    let pollenHint = (cm, options) => {
      return new Promise(resolve => {
        let cursor = cm.getCursor(),
            line = cm.getLine(cursor.line);

        let start = cursor.ch, end = cursor.ch;
        while (start && /\S/.test(line.charAt(start-1)))
          --start;

        let result = [];
        if (line.charAt(start) != this.getPollenSetup('command-char')) {
          return resolve(null);
        } else {
          start += 1;
          let prefix = line.substring(start, end);

          for (let name in this.pollenTags) {
            if (name.startsWith(prefix))
              result.push(name);
          }
          return resolve({
            list: result,
            from: CodeMirror.Pos(cursor.line, start),
            to: CodeMirror.Pos(cursor.line, end)
          });
        }
      });
    };
    editor.setOption("hintOptions", {
      hint: pollenHint
    });
  }

  /**
   * Pollen setup has variables defined in the module 'pollen/setup'. User
   * can override values in pollen.rkt. Use this method to get correct value.
   */
  getPollenSetup(name) {
    return this.pollenSetup[name] || this.pollenSetup[`default-${name}`];
  }

  /**
   * This method saves text in editor.
   */
  save() {
    return new Promise((resolve, reject) => {
      if (this.editor.isClean(this.docGeneration)) {
        // pretend RPC succeeded
        resolve(new RPCResultVal(0, true));
      }

      this.docGeneration = this.editor.changeGeneration();
      let resource = this.resource;
      let text = this.editor.getValue();
      this.rpc.call_server("save", text, resource)
        .then(resolve).catch(reject);
    });
  }

  makeAutoSaveHandler() {
    var scheduledSave;
    return () => {
      if (scheduledSave) {
        clearTimeout(scheduledSave);
      }

      scheduledSave = setTimeout(() => {
        this.save()
          .then(v => 'saved')
          .catch(v => 'error')
          .then(v => this.saveStatusChangeEvent.notify(v));
      }, 2000);
    };
  }

  renderPreview() {
    return this.rpc.call_server("render", this.resource).then(rpcVal => {
      let path = rpcVal.result;
      this.previewReadyEvent.notify(path);
    }).catch(rpcVal => {
      notifyError(rpcVal.error);
    });
  }

  changeEditorSettings(obj) {
    for (let name in obj) {
      if (obj.hasOwnProperty(name)) {
        this.editorSettings.value(name, obj[name]);
        console.log(`set editor option ${name} to ${obj[name]}`);
        this.editor.setOption(name, obj[name]);
      }
    }
    this.refreshEditor();
    this.editorSettingsChangeEvent.notify(obj);
  }

  // TODO: editor mode should return such a check function
  syntaxCheck() {
    let lastLine = this.editor.doc.lastLine();
    let state = this.editor.getStateAfter(lastLine, true);
    return state.braceStack.length == 0;
  }
}

class Keymap {
  constructor(key, func, doc) {
    this.key = key;
    this.func = func;
    this.doc = doc;
  }
}

/**
 * We can simply construct a key-value object to present
 * editorSettings, but we also need all options for each value for
 * customization in UI. This class helps construct this info.  It also
 * can serialize all settings to use in CodeMirror and local storage.
 *
 * obj = EditorSettingsModel()
 * obj.value('theme'): get value of theme
 * obj.value('theme', 'x'): set value of theme
 * obj.options('theme'): get all available value for setting theme
 * obj.serialize(): get a simple object representing editor settings
 */
class EditorSettingsModel {
  constructor() {
    let makeOptions = (list, defaultIndex=0) => ({
      value: list[defaultIndex],
      options: list
    });
    let boolOption = makeOptions([true, false]);
    this.settings = {
      autofocus: boolOption,
      matchBrackets: boolOption,
      lineWrapping: boolOption,
      scrollbarStyle: makeOptions(["null"]),
      theme: makeOptions([
        "default", "solarized light", "solarized dark"
      ]),
      mode: makeOptions(["pollenMixed"]),
      autoReloadPreview: makeOptions([false, true])
    };

  }

  hasKey(key) {
    return this.settings.hasOwnProperty(key);
  }

  keys() {
    let result = [];
    for (let name in this.settings) {
      if (this.hasKey(name))
        result.push(name);
    }
    return result;
  }

  value(name, val) {
    if (val) {
      this.settings[name].value = val;
    } else {
      val = this.settings[name].value;
    }
    return val;
  }

  options(name) {
    return this.settings[name].options;
  }

  serialize() {
    let obj = Object.assign({}, DEFAULT_EDITOR_SETTINGS);

    for (let name of this.keys()) {
      obj[name] = this.value(name);
    }
    return obj;
  }
}
