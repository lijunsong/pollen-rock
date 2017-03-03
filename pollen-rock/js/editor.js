// This is an application using a self-made Model-Control-View
// principle.

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
    },
    shellRequest : function(cmd) {
      return {
        type : 'shell',
        cmd  : cmd
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
      $.post(server_api, initRequest, function(config) {
        // continue init
        var jsconfig = JSON.parse(config);
        jsconfig["resource"] = resource;
        model.initPollenConfig(jsconfig["config"]);
        model.initPollenTags(jsconfig["tags"]);

        editorView.init(function(name) {
          return ctrl.getPollenConfig(name);
        });

        // Editor is initialized. Now it's safe to start
        // auto saving.
        setInterval(function() {
          ctrl.save();
        }, 2000);

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
      this.initAutoReload();
      shellView.init();
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
      preview.showLoader();
      $.post(server_api, request, function(status) {
        preview.reload(renderedResource);
      }).fail(function(status) {
        notifyView.error("server error");
      }).always(function() {
        preview.hideLoader();
      });
    },

    initAutoReload : function() {
      setInterval(function() {
        if (preview.visible) {
          // auto-reload will save the current generation.
          // and check the generation before rendering.
          // XXX: this solution works but is not perfect. Need a method
          // to work with auto-save.
          var editor = editorView.editor;
          if (editor.isClean(editor.renderedGen))
            return;
          editor.renderedGen = editor.changeGeneration();
          ctrl.renderPreview();
        }
      }, 5000);
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
          return function() {
            document.documentElement[f[i]]();
          };
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

        defineKey('Shift-Ctrl-P',
                  changePreviewLayout,
                  "Open/Close Preview Window"),
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
      this.view.removeClass("push-m3");
      this.view.addClass("push-m6");
      // editor's size change would misplace the cursor.
      this.refresh();
    },

    toCenter : function() {
      this.view.removeClass("push-m6");
      this.view.addClass("push-m3");
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
      var wrapper = $("#preview-wrapper");
      this.wrapper = wrapper;
      this.frame = $("#preview-frame");
      this.loader = $("#preview-loader");
      if (! wrapper.hasClass("hide"))
        wrapper.addClass("hide");
      this.visible = ! wrapper.hasClass("hide");
      $('#previewBtn').click(changePreviewLayout);
    },

    toggleHide: function() {
      this.wrapper.toggleClass("hide");
      this.visible = ! this.wrapper.hasClass("hide");
    },

    showLoader: function() {
      this.loader.removeClass("hide");
    },

    hideLoader: function() {
      this.loader.addClass("hide");
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

  var changePreviewLayout = function() {
    preview.toggleHide();
    if (preview.visible) {
      editorView.toRight();
      ctrl.renderPreview();
    } else {
      editorView.toCenter();
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
      this.show();
    },
    hide : function() {
      this.loader.addClass("hide");
    },
    show : function() {
      this.loader.removeClass("hide");
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

  var shellView = {
    init : function() {
      this.view = $("#shell-wrapper");
      this.outputTemplate = $('script[data-template="shell-output"]').html();
      this.outputWrapper = $('#shell-output-wrapper');

      //this.view.addClass("hide");
      $("#shellBtn").click(function() {
        shellView.view.toggleClass("hide");
      });

      $("#shell-clean").click(function() {
        shellView.outputWrapper.empty();
      });

      $("#shell-input").keypress(function(e) {
        if (e.which == 10 || e.which == 13) {
          var input = this;
          var originValue = input.value;
          var req = model.shellRequest(originValue);
          input.value = '';
          $.post(server_api, req, function(result) {
            var tmp = shellView.outputTemplate
                .replace(/{{input}}/, originValue)
                .replace(/{{output}}/, result);
            shellView.outputWrapper.append(tmp);
          }).fail(function(status) {
            var tmp = shellView.outputTemplate
                .replace(/{{input}}/, originValue)
                .replace(/{{output}}/, status.statusText);
            shellView.outputWrapper.append(tmp);
          }).always(function() {
            shellView.outputWrapper.scrollTop(shellView.outputWrapper[0].scrollHeight);
          });
        }
      });
    }
  };

  ctrl.init();

  $("#shellBtn").click(function() {
    var req = model.shellRequest("sleep 100 && ls");
    $.post(server_api, req, function(result) {
      console.log(result);
    }).fail(function() {
      console.log("failed");
    });
  });
});
