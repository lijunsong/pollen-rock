$(document).ready(function() {
  "use strict";

  var server_api = "/api";

  var model = {
    initPollenConfig : function(config) {
      this["pollenConfig"] = config;
    },

    initPollenTags : function(tags) {
      this["pollenTags"] = tags;
    },

    saveRequest : function(text, resource) {
      return {
        type : 'save',
        text : text,
        resource : resource
      };
    },
    renderRequest : function(resource) {
      return {
        type : 'render',
        resource : resource
      };
    },
    pollenConfigRequest : function(resource) {
      return {
        type : 'config',
        resource : resource
      };
    }
  };

  ////////////////////////////////////////////////////
  // ctrl is the bridge between the model and views //
  ////////////////////////////////////////////////////
  var ctrl = {
    // ctrl will request user's pollen config before initializing the
    // rest
    init : function() {

      var resource = $("#editor").attr("data");
      var initRequest = model.pollenConfigRequest(resource);
      loaderView.init();
      loaderView.show();
      $.post(server_api, initRequest, function(config) {
        // continue init
        var jsconfig = JSON.parse(config);
        jsconfig["resource"] = resource;
        model.initPollenConfig(jsconfig["config"]);
        model.initPollenTags(jsconfig["tags"]);

        editorView.init(function(name) {
          return ctrl.getPollenConfig(name);
        });

        notifyView.info("Ready to Rock!");
      }).fail(function(status) {
        notifyView.error(status.statusText);
      }).always(function() {
        loaderView.hide();
      });

      materializeView.init();
      preview.init();
      saveStatusView.init();
      panelView.init();

      setInterval(function() {
        ctrl.save();
      }, 2000);
    },

    save : function() {
      var editor = editorView.editor;
      if (editor.isClean(editor.curGen)) {
        return;
      }
      editor.curGen = editor.changeGeneration();
      saveStatusView.saving();
      var resource = this.getPollenConfig("resource");
      var text = editor.getValue();
      var request = model.saveRequest(text, resource);
      $.post(server_api, request, function(status) {
        saveStatusView.saved();
      }).fail(function(status) {
        saveStatusView.error();
      });
    },

    renderPreview : function() {
      var resource = this.getPollenConfig("resource");
      var request = model.renderRequest(resource);
      var renderedResource = this.getPollenConfig("rendered-resource");
      $.post(server_api, request, function(status) {
        preview.reload(renderedResource);
      }).fail(function(status) {
        notifyView.error("server error");
      });
    },

    getPollenConfig : function(name) {
      // XXX: it's probably better to let server
      // decide the logic.
      var val = model.pollenConfig[name];
      return val || model.pollenConfig['default-'+name];
    },

    getPollenTag : function(name) {
      return model.pollenTags[name];
    }
  };

  ////////////////////////////
  // create editor instance //
  ////////////////////////////
  var editorView = {
    init : function(getPollenConfig) {
      this.view = $("#editor-wrapper");
      var area = document.getElementById("compose");
      this.editor = CodeMirror.fromTextArea(area, {
        autofocus: true,
        matchBrackets: true,
        lineWrapping: true,
        mode: 'pollen'
      });
      this.fullscreen = this.initFullscreen();
      this.initKeyMaps(getPollenConfig);
      this.initEventHandlers();

      // ready to show
      this.view.removeClass("hide");
      this.refresh();
      return this.editor;
    },

    refresh : function() {
      this.editor.refresh();
    },

    initEventHandlers : function() {
      this.editor.on("change", function(obj) {
        saveStatusView.empty();
      });
    },

    initFullscreen : function() {
      var fscreen = false;
      var f = ["requestFullscreen", "mozRequestFullScreen",
               "webkitRequestFullscreen", "msRequestFullscreen"];
      for (var i = 0; i < f.length; i++) {
        if (f[i] in document.documentElement) {
          return function() {document.documentElement[f[i]]();};
        }
      }
      return false;
    },

    initKeyMaps: function(getPollenConfig) {
      // return an object containing doc that be used to
      // build up keyMap hint later.
      var view = this;
      function defineKey(key, func, doc) {
        return {
          key: key,
          doc: doc,
          func: func
        };
      }

      var commandChar = getPollenConfig("command-char");
      var keyMaps = [
        defineKey('Shift-2', function(e) {
          var pos = e.getCursor();
          if (pos.ch == 0)
            e.replaceSelection(commandChar);
          else {
            var lastPos = CodeMirror.Pos(pos.line, pos.ch-1);
            if (e.getRange(lastPos, pos) == commandChar)
              e.replaceRange('@', lastPos, pos);
            else
              e.replaceSelection(commandChar);
          }
        }, "Insert Command Char or @"),

        defineKey('Cmd-Enter', function() {
          if (view.fullscreen) {
            view.fullscreen();
          } else {
            notifyView.error("Your Browser Doesn't Support Fullscreen");
          }
        }, "Enter Fullscreen"),

        defineKey('Shift-Ctrl-P', function() {
          preview.toggleHide();
          if (preview.visible) {
            ctrl.renderPreview();
            view.toRight();
          } else {
            view.toCenter();
          }
        }, "Open/Close Preview Window")
      ];

      for (var i = 0; i < keyMaps.length; i++) {
        var m = {};
        m[keyMaps[i].key] = keyMaps[i].func;
        this.editor.addKeyMap(m);
      }

      keymapView.init();
      keymapView.render(keyMaps);
    },

    toRight : function() {
      this.view.addClass("side-right");
      // editor's size change would misplace the cursor.
      this.refresh();
    },

    toCenter : function() {
      this.view.removeClass("side-right");
      // editor's size change would misplace the cursor.
      this.refresh();
    }
  };

  var saveStatusView = {
    init : function() {
      this.view = $("#savestatus");
      this.empty();
    },

    saved : function() {
      this.view.text("saved");
    },

    saving : function() {
      this.view.text("saving");
    },

    empty : function() {
      this.view.text('');
    },

    error : function(text) {
      notifyView.error("Failed to save.");
    }

  };

  ///////////////////
  // Setup Preview //
  ///////////////////
  var preview = {
    init : function() {
      var wrapper = $("#previewwrapper");
      this.wrapper = wrapper;
      this.frame = $("#preview");
      if (! wrapper.addClass("hide"))
        wrapper.addClass("hide");
      this.visible = ! wrapper.hasClass("hide");
    },

    toggleHide: function() {
      this.wrapper.toggleClass("hide");
      this.visible = ! this.wrapper.hasClass("hide");
    },

    reload: function(src) {
      if (this.frame.attr('src')) {
        var doc = this.frame.contents()[0];
        doc.location.reload(true);
      } else {
        this.frame.attr('src', src);
      }
    }
  };

  var notifyView = {
    info : function(msg) {
      Materialize.toast(msg, 4000, 'toast-info');
    },
    error : function(msg) {
      Materialize.toast(msg, 4000, 'toast-error');
    }
  };

  var loaderView = {
    init : function() {
      this.loader = $("#loader");
    },
    hide : function() {
      this.loader.removeClass("active");
    },
    show : function() {
      this.loader.addClass("active");
    }
  };

  var keymapView = {
    init: function() {
      this.rowTemplate = $('script[data-template="keymap-row"]').html();
      this.modal = $("#keymap-settings");
      this.tableBody = $("#keymap-body");
    },

    open: function() {
      this.modal.modal("open");
    },

    render: function(keymaps) {
      var template = this.rowTemplate;
      var body = this.tableBody;
      keymaps.forEach(function (km) {
        var row = template
            .replace(/{{keystroke}}/g, km.key)
            .replace(/{{command}}/g, km.doc);
        body.append(row);
      });
    }
  };

  var materializeView = {
    init: function() {
      $(".modal").modal();
    }
  };

  var panelView = {
    init: function() {
      this.panel = $("#editor-panel");
    },
    hide: function() {
      this.panel.addClass("hide");
    },
    show: function() {
      this.panel.removeClass("hide");
    }
  };

  ctrl.init();
});
