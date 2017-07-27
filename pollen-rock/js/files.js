"use strict";

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
  /**
   * Model contains file information.
   *
   * resource points to the folder in which current page is indexing
   */
  constructor(resource) {
    this.rpc = new PollenRockRPC("/api");
    this.resource = resource;
    this.files = {};

    // Events
    this.fileListReadyEvent = new Event(this);
  }

  fetchList(resource) {
    this.rpc.call_server("ls", resource).then(v => {
      this.files = v.result;
      this.fileListReadyEvent.notify(this.files);
    }).catch(err => {
      notifyError(err.error);
    });
  }
}

class View {
  constructor(model) {
    this.model = model;

    this.setupBindings()
      .setupHandlers()
      .attach();
  }

  setupBindings() {
    this.$folderCollection = $("#folder-indexing");
    this.$fileCollection = $("#file-indexing");
    return this;
  }

  setupHandlers() {
    this.fileListReadyHandler = this.refreshFileList.bind(this);
    return this;
  }

  attach() {
    this.model.fileListReadyEvent.attach(this.fileListReadyHandler);
    return this;
  }

  refreshFileList(sender, files) {
    console.log(files);
  }
}

class Controller {
  constructor(model, view) {
    this.model = model;
    this.view = view;
  }

  init() {
    this.model.fetchList(this.model.resource);
  }
}

$(document).ready(() => {
  let resource = window.location.pathname;
  let model = new Model(resource);
  let view = new View(model);
  let controller = new Controller(model, view);
  controller.init();
});
