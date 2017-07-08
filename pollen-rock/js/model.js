"use strict";

function notifyInfo(msg) {
  Materialize.toast(msg, 4000, 'toast-info');
}

function notifyError(msg) {
  console.error(msg);
  Materialize.toast(msg, 4000, 'toast-error');
}


class Model {
  constructor($textarea, resource) {
    this.resource = resource;

    this.rpc = new PollenRockRPC("/api");
    this.pollenSetup = {};
    this.pollenTags = {};

    this.editor = CodeMirror.fromTextArea($textarea, {
      autofocus: true,
      matchBrackets: true,
      lineWrapping: true,
      scrollbarStyle: "null",
      theme: "default",
      mode: 'pollen'
    });
    this.docGeneration = null;
    this.editorPreference = {};
    this.keyMaps = new Map();

    // Events
    this.editorInitFailEvent = new Event(this);
    this.pollenTagsReadyEvent = new Event(this);
    this.pollenSetupReadyEvent = new Event(this);
    this.saveStatusChangeEvent = new Event(this);
    this.previewReadyEvent = new Event(this);
    this.keymapUpdateEvent = new Event(this);

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

    return this;
  }

  init() {
    this.rpc.call_server("get-project-config", this.resource).then(v => {
      let config = v.result;
      this.pollenTags = config['tags'];
      this.pollenTagsReadyEvent.notify(this.pollenTags);
      this.pollenSetup = config['setup'];
      this.pollenSetupReadyEvent.notify(this.pollenSetup);
    }).catch(err => {
      this.editorInitFailEvent.notify(err);
    });
  }

  refreshEditor() {
    this.editor.refresh();
  }

  /**
   * add key maps to the editor
   */
  addKeyMap(keymap_list) {
    for (let km of keymap_list) {
      this.keyMaps.set(km.key, km);
    }
    let kms = {};
    for (let [k, v] of this.keyMaps) {
      kms[k] = v.func;
    }
    this.editor.setOption("extraKeys", kms);
    this.keymapUpdateEvent.notify(this.keyMaps);
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
   * TODO: resolve and reject should be passed object representing RPC result
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
      notifyError(`render failed: ${rpcVal.error}`);
    });
  }
}

class Keymap {
  constructor(key, func, doc) {
    this.key = key;
    this.func = func;
    this.doc = doc;
  }
}
