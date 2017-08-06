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
    this.resource = resource.replace(/%20/g, ' ');
    this.pollenSuffix = [".pm", ".p", ".pp", ".ptree"];
    this.files = {};

    // notify with an object containing directory and non-directory
    // list when files are feteched
    this.fileListReadyEvent = new Event(this);
    // notify with an object indicating whether fop (filesystem
    // operation) succeeded. The key of this object is the same as
    // that passed into Vew.fsEvent, with one more bool property
    // 'result' added.
    this.fopResultReadyEvent = new Event(this);
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

  // check filename. If anything wrong, throw exception
  checkFilename(filename) {
    if (!filename || filename.length == 0) {
      throw `${filename} is empty`;
    }
  }

  // accept the same argument as View.fsEvent
  fop(obj, curDir) {
    let op = obj.op;
    let opArgList;
    switch (obj.op) {
    case 'mv':
      opArgList = [obj.src, obj.dst];
      break;
    case 'touch':
    case 'mkdir':
    case 'rm':
      opArgList = [obj.src];
      break;
    default:
      notifyError(`Uknown fs operation ${obj.op}`);
      throw obj;
    }

    let opArgPathList = opArgList.map(a => `${curDir}/${a}`);
    for (let a of opArgPathList) {
      this.checkFilename(a);
    }

    console.log(`calling ${op} with ${opArgPathList} on server`);
    this.rpc.call_server(op, ... opArgPathList).then(v => {
      obj['result'] = v.result;
      this.fopResultReadyEvent.notify(obj);
    }).catch(err => {
      notifyError(err.error);
      obj['result'] = false;
      this.fopResultReadyEvent.notify(obj);
    });
  }
}

class View {
  constructor(model) {
    this.model = model;
    this.hostname = window.location.host;

    /// Events for controller to subscribe

    // notify with an object there has the following property
    // op := mv | rm | touch | mkdir
    // src := string
    // dst := [string]
    this.fsEvent = new Event(this);

    this.setupBindings()
      .setupHandlers()
      .attach();
  }

  setupBindings() {
    this.$wrapper = $("#wrapper");
    this.$foldersWrapper = $("#folder-wrapper");
    this.$filesWrapper = $("#file-wrapper");
    this.$itemTemplate = $("#tpl-collection-item");
    this.$itemRightTemplate = $("#tpl-item-right");
    this.$renameInputTemplate = $("#tpl-rename-input-field");
    this.$addTemplate = $("#tpl-add-file-or-folder-input");

    this.$addFolderIcon = $("#add-folder");
    this.$addFileIcon = $("#add-file");
    return this;
  }

  setupHandlers() {
    let self = this;
    this.fileListReadyHandler = this.refreshFileList.bind(this);
    this.fopResultReadyHandler = this.fopResultReady.bind(this);

    return this;
  }

  attach() {
    this.model.fileListReadyEvent.attach(this.fileListReadyHandler);
    this.model.fopResultReadyEvent.attach(this.fopResultReadyHandler);

    this.$addFolderIcon.on('click', this.makeAddFileOrFolderHandler("mkdir"));
    this.$addFileIcon.on('click', this.makeAddFileOrFolderHandler("touch"));
    return this;
  }

  // makeAddFileOrFolderHandler returns a callback for click event on add
  // file/folder icons.
  //
  // op must be either mkdir or touch
  makeAddFileOrFolderHandler(op) {
    let self = this;
    return function () {
      let $header = $(this).parents('.collection-header');
      if ($header.hasClass('adding-mode')) {
        return;
      }
      $header.addClass('adding-mode');
      let $newItem = $(self.$addTemplate.html());
      $header.after($newItem);

      // setup label
      let fileType = op == "mkdir" ? "folder" : "file";
      $newItem.find('label').text(`Enter a name and then press Enter to create a ${fileType}`);

      // setup click handlers
      $newItem.find('.op').on('click', () => {
        $header.removeClass('adding-mode');
        $newItem.remove();
      });
      // setup Enter handlers
      $newItem.find('input').on('change', function() {
        let filename = $(this).val();
        console.log(`create: ${filename}`);
        self.fsEvent.notify({
          op: op,
          src: filename
        });
      });
    };
  }


  /**
   * setup handlers for filesystem operations. Because icons are
   * constructed dynamically, we don't call this function in
   * constructor
   */
  setupFsHandler() {
    let self = this;
    $(".expand-more").on('click', function() {
      $(this).parent().addClass('expanding');
    });
    $(".expand-less").on('click', function() {
      $(this).parent().removeClass('expanding');
    });
    $(".fs-delete").on('click', function() {
      let $this = $(this);
      if ($this.hasClass('rm-mode')) {
        let filename = $this.parents('.collection-item').attr('data');
        self.fsEvent.notify({
          op: 'rm',
          src: filename
        });
      }
      $this.addClass('rm-mode');
      setTimeout(() => {
        $this.removeClass('rm-mode');
      }, 3000);
    });
    $(".fs-rename").on('click', function() {
      let $collectionItem = $(this).parents('.collection-item');
      $collectionItem.addClass('mv-mode');
      self.renameHandler($collectionItem);
    });
    $(".rename-cancel").on('click', function() {
      $(this).parents('.collection-item').removeClass('mv-mode');
    });
  }

  addFileHandler() {

  }

  addFileOrFolderHandler($collection) {
  }

  renameHandler($item) {
    if (! $item.hasClass('collection-item')) {
      notifyError('internal Error: renameHandler');
      console.log($item);
      return;
    }

    let $itemLeft = $item.find('.item-left-renaming');
    let filename = $item.attr('data');

    let $input = $(this.$renameInputTemplate.html());
    $input.find('input').attr('value', filename);
    $input.find('label').addClass('active');
    $input.find('span').text(filename);

    $itemLeft.html('').append($input);
    console.log(`rename ${filename}`);

    let self = this;
    $input.find('input').on('change', function() {
      let newName = $(this).val();
      self.fsEvent.notify({
        op: 'mv',
        src: filename,
        dst: newName
      });
    });
  }

  fopResultReady(sender, obj) {
    if (obj.result === false) {
      notifyError(`${obj.op} failed`);
    } else {
      document.location.reload();
    }
  }

  setupTooltip() {
    $('.tooltipped').tooltip({delay: 300});
  }

  icon(name, attr) {
    let $i = $("<i>", attr).text(name);
    $i.addClass('material-icons');
    return $i;
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

  make$FolderList(folders) {
    let lists = folders || [];
    let result = [];

    for (let name of lists) {
      let url = this.makeFileURL(name);
      let itemLeft = $('<a>', {href: url, text: name});
      let item = $(this.$itemTemplate.html()).attr('data', name);
      item.find('.item-left').append(itemLeft);
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
      let itemRight;
      let isSource = this.model.isPollenSource(name);
      if (isSource) {
        // add pollen source operations
        let editOp = $(this.$itemRightTemplate.html())
            .attr('data-tooltip', 'open editor to edit this file')
            .append($('<a>', {href: `/edit${url}`})
                    .append(this.icon('code')));
        let viewOp = $(this.$itemRightTemplate.html())
            .attr('data-tooltip', 'render this file')
            .append($('<a>', {href: `/watchfile${url}`})
                    .append(this.icon('visibility')));
        itemRight = [editOp, viewOp];
      }

      let item = $(this.$itemTemplate.html())
          .attr('data', name)
          .addClass(isSource ? "is-pollen-source" : "not-pollen-source");
      item.find('.item-left').append(itemLeft);
      item.find('.item-right').prepend(itemRight);
      result.push(item);
    }
    return result;
  }

  refreshFileList(sender, fileList) {
    let folders = this.make$FolderList(fileList['directory']);
    let files = this.make$RegularFileList(fileList['non-directory']);

    this.$foldersWrapper.find('ul').append(folders);
    this.$filesWrapper.find('ul').append(files);

    this.setupFsHandler();
    this.setupTooltip();
  }
}

class Controller {
  constructor(model, view) {
    this.model = model;
    this.view = view;

    this.setupBindings()
      .setupHandlers()
      .attach();
  }

  setupBindings() {
    return this;
  }

  setupHandlers() {
    self.fsHandler = this.fs.bind(this);
    return this;
  }

  attach() {
    this.view.fsEvent.attach(self.fsHandler);
    return this;
  }

  fs(sender, obj) {
    this.model.fop(obj, this.model.resource);
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
