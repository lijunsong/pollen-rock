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
    this.pollenSuffix = [".pm", ".p", ".pp", ".ptree"];
    this.files = {};

    // Events
    this.fileListReadyEvent = new Event(this);
  }

  fetchList(resource) {
    this.rpc.call_server("ls", resource).then(v => {
      this.files = v.result;
      this.fileListReadyEvent.notify(this.files);
    })/*.catch(err => {
      notifyError(err.error);
    });*/
  }

  isPollenSource(filename) {
    for (let suf of this.pollenSuffix) {
      if (filename.endsWith(suf)) {
        return true;
      }
    }
    return false;
  }
}

class View {
  constructor(model) {
    this.model = model;
    this.hostname = window.location.host;

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

  /**
   * There are several things involved when creating a item for
   * showing in a list, so make these steps a separate function.
   */
  buildCollectionItem(filename) {
    let base = this.model.resource;
    base = (base == "/" ? "" : base);
    let href = `${base}/${filename}`;

    if (this.model.isPollenSource(filename)) {
      // redirect to edit page and add watch icon
      href = `/edit${href}`;
    }
    let item = $("<a>")
        .attr('href', href)
        .attr('class', 'collection-item')
        .text(filename);
    return item;
  }

  refreshFileList(sender, fileList) {
    let self = this;
    function addItems($collection, nameList) {
      for (let name of nameList) {
        let item = self.buildCollectionItem(name);
        $collection.append(item);
      }
    }
    let dirs = fileList['directory'];
    let files = fileList['non-directory'];
    this.$folderCollection.html('');
    this.$fileCollection.html('');
    addItems(this.$folderCollection, dirs);
    addItems(this.$fileCollection, files);
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
