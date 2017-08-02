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
    this.$wrapper = $("#wrapper");
    this.$indexWrapperTemplate = $("#tpl-index-wrapper");
    this.$itemTemplate = $("#tpl-collection-item");
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

  icon(name) {
    return $("<i>").attr('class', 'material-icons').text(name);
  }

  makeFileURL(filename){
    let base = this.model.resource;
    let href;
    if (base == "/") {
      href = `/${filename}`;
    } else {
      href = `${base}/${filename}`;
    }
    return href;
  }

  /**
   * Common file operations may include rename and delete.
   * This method returns a list of jquery object
   */
  makeCommonFileOperations() {
    let icon = this.icon('more_horiz');
    return [icon];
  }

  make$FolderList(folders) {
    let lists = folders || [];
    let result = [];

    for (let name of lists) {
      let url = this.makeFileURL(name);
      let itemRight = this.makeCommonFileOperations();
      let itemLeft = $('<a>', {href: url, text: name});
      let item = $(this.$itemTemplate.html());
      item.find('.item-left').append(itemLeft);
      item.find('.item-right').append(itemRight);
      result.push(item);
    }
    return result;
  }

  make$RegularFileList(files) {
    let lists = files || [];
    let result = [];

    for (let name of lists) {
      let url = this.makeFileURL(name);
      let itemLeft = name;
      let commonOps = this.makeCommonFileOperations();
      let itemRight;
      let isSource = this.model.isPollenSource(name);
      if (! isSource) {
        itemRight = commonOps;
      } else {
        // add pollen source operations
        let editOp = $('<a>', {href: `/edit${url}`})
            .append(this.icon('edit'));
        let viewOp = $('<a>', {href: `/watchfile${url}`})
            .append(this.icon('pageview'));
        itemRight = [editOp, viewOp, ... commonOps];
      }

      let item = $(this.$itemTemplate.html())
          .addClass(isSource ? "is-pollen-source" : "not-pollen-source");
      item.find('.item-left').append(itemLeft);
      item.find('.item-right').append(itemRight);
      result.push(item);
    }
    return result;
  }

  refreshFileList(sender, fileList) {
    this.$wrapper.html('');

    let folders = this.make$FolderList(fileList['directory']);
    let files = this.make$RegularFileList(fileList['non-directory']);

    let listNames = {
      'Folders' : folders,
      'Files': files
    };
    for (let itemListName of Object.keys(listNames)) {
      let itemList = listNames[itemListName];
      if (itemList.length != 0) {
        let $listWrapper = $(this.$indexWrapperTemplate.html());
        $listWrapper.find('ul').append(itemList);
        $listWrapper.find('.indexing-name').text(itemListName);
        this.$wrapper.append($listWrapper);
      }
    }
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
