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
  console.log(msg);
  msg = msg.replace(/\n/g, '<br />');
  Materialize.toast(msg, 4000, 'toast-info');
}

function notifyError(msg) {
  console.error(msg);
  msg = msg.replace(/\n/g, '<br />');
  Materialize.toast(msg, 15000, 'toast-error');
}

class Model {
  constructor($textarea, resource) {
    this.resource = resource;
    this.$textarea = $textarea;

    this.rpc = new PollenRockRPC("/api");
    this.pollenSetup = {};
    this.pollenTags = {};

    /// Events
    // notify with an object containing changed key and value
    this.editorSettingsChangeEvent = new Event(this);
    // notify with pollenTags object (has all tag functions)
    this.pollenTagsReadyEvent = new Event(this);
    // notify with pollenSetup object (has all project setup)
    this.pollenSetupReadyEvent = new Event(this);
    // notify with saveStatus string
    this.saveStatusChangeEvent = new Event(this);
    // notify with null
    this.previewReadyEvent = new Event(this);
    // notify with a new Map
    this.keymapChangeEvent = new Event(this);
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

    return this;
  }

  fetchConfigs() {
    this.rpc.call_server("get-pollen-setup", this.resource).then(v => {
      this.pollenSetup = v.result;
      this.pollenSetupReadyEvent.notify(this.pollenSetup);
    }).catch(err => {
      notifyError(err);
    });

    this.rpc.call_server("get-pollen-tags", this.resource).then(v => {
      this.pollenTags = v.result;
      this.pollenTagsReadyEvent.notify(this.pollenTags);
    }).catch(err => {
      notifyError(err);
    });
  }

  initEditor(editorSettings, keymap_list) {
    this.docGeneration = null;
    this.keyMaps = new Map();

    this.editor = CodeMirror.fromTextArea(this.$textarea, editorSettings);
    // setup auto complete
    this.setupEditorHint(this.editor);
    // setup key bindings
    this.addCMKeyMap(keymap_list);

    // Handlers
    this.setupHandlers()
      .attach();
  }

  refreshEditor() {
    // refresh the editor only when editor has been initialized (created)
    if (this.editor) {
      this.editor.refresh();
    }
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

          for (let name of Object.keys(this.pollenTags)) {
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
    for (let name of Object.keys(obj)) {
      console.log(`set editor option ${name} to ${obj[name]}`);
      this.editor.setOption(name, obj[name]);
    }
    this.refreshEditor();
    this.editorSettingsChangeEvent.notify(obj);
  }

  // Check syntax using syntax check function of current editor mode
  syntaxCheck() {
    let mode = this.editor.doc.getMode();
    let check = CodeMirror.syntaxCheck[mode.name];
    if (! check) {
      console.log(`syntaxCheck not found for mode ${mode.name}.`);
      return false;
    }
    return check(this.editor, this.editor.doc.lastLine());
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
 * obj.serialize(): get a key-value object representing editor settings
 */
// Given a model instance, creating an editor settings that contains
// default settings.
class EditorSettingsModel {
  constructor(model) {
    this.model = model;
    this.modeMap = {
      '.html': 'xml',
      '.html.p': 'xml',
      '.rkt': 'scheme',
      '.pm': 'pollenMixed',
      '.p': 'pollen',
      '.pp': 'pollen',
      '.ptree': 'pollenMixed'
    };

    this.uncustomizableSettings = {
      // uncustomizable settings
      addModeClass: true,
      autofocus: true,
      matchBrackets: true,
      "command-char": this.model.getPollenSetup('command-char') || '◊'
    };

    this.settings = {
      // customizable settings
      lineWrapping: true,
      lineNumbers: false,
      autoReloadPreview: true,
      theme: {
        value: "default",
        options: new Set(["default", "solarized light", "solarized dark"])
      },
      font: 'Source Sans Pro',
      indentUnit: 2,
      mode: {
        value: this.searchMode(this.model.resource),
        options: new Set(Object.values(this.modeMap))
      }
    };
  }

  searchMode(filename) {
    for (let suffix of Object.keys(this.modeMap)) {
      if (filename.endsWith(suffix)) {
        return this.modeMap[suffix];
      }
    }
    return 'pollen';
  }

  hasKey(key) {
    return this.settings.hasOwnProperty(key);
  }

  keys() {
    return Object.keys(this.settings);
  }

  value(name, val) {
    if (! val) {
      let v = this.settings[name];
      return v.value || v;
    }

    switch (typeof(v)) {
    case 'string':
    case 'boolean':
    case 'number':
      this.settings[name] = v;
      break;
    case 'object':
      if (v.options.has(val)) {
        this.settings[name].value = v;
        return null;
      }
    default:
      throw `unknown value ${v}`;
    }
  }

  options(name) {
    let v = this.settings[name].options;
    if (! v && typeof(this.settings[name]) == 'boolean') {
      v = [true, false];
    }
    return v;
  }

  serialize() {
    let obj = Object.assign({}, this.uncustomizableSettings);

    for (let name of this.keys()) {
      obj[name] = this.value(name);
    }
    return obj;
  }
}
