// This is an application using a self-made Model-Control-View
// principle.

$(document).ready(function() {
  "use strict";

  var server_api = "/api";

  var rpc = new PollenRockRPC("/api");

  var model = {
    init : function() {
      // when multiple rendering requests on the fly, the first response will
      // hide others' preview loader. This variable records on-the-fly
      // rendering requests.
      this.renderOnTheFly = 0;
      // save status is among "saving", "saved", or ""
      // saveStatus is to protect document from closing browser
      this.saveStatus = {
        val : "saved",
        saving: function() { this.val = "saving"; return this.val; },
        saved:  function() { this.val = "saved";  return this.val;},
        clean:  function() { this.val = ""; return this.val; },
        isSaved: function() { return this.val == "saved"; }
      };

      // editor preference
      this.preference = this.initPreference();
    },

    initPreference: function() {
      var options = {
        autofocus: true,
        matchBrackets: true,
        lineWrapping: true,
        scrollbarStyle: "null",
        theme: "default",
        mode: 'pollen'
      };
      return options;
    },

    initPollenConfig : function(config) {
      this["pollenConfig"] = config;
    },

    initPollenTags : function(tags) {
      this["pollenTags"] = tags;
    },

    initRockConfig : function(config) {
      this["rockConfig"] = config;
    }
  };

  ////////////////////////////////////////////////////
  // ctrl is the bridge between the model and views //
  ////////////////////////////////////////////////////
  var ctrl = {
    // ctrl will request user's pollen config before initializing the
    // rest
    init : function() {
      // initialize model
      model.init();

      // initialize Views.
      var resource = $("#compose").attr("data");
      loaderView.init();
      materializeView.init();
      preview.init();
      saveStatusView.init();
      preferenceView.init();

      // initialize project specific settings
      rpc.call_server("get-project-config", resource).then(v => {
        let jsconfig = v.result;
        model.initPollenConfig(jsconfig["pollenConfig"]);
        model.initPollenTags(jsconfig["tags"]);
        model.initRockConfig(jsconfig["rockConfig"]);

        editorView.init(function(name) {
          return ctrl.getPollenConfig(name);
        });
        panelView.init(ctrl.getRockConfig("no-shell"));
        upperWrapperView.init(editorView.editor.getWrapperElement());
        notifyView.info("Ready to Rock!");
      }).catch(err => {
        notifyView.error(err);
      }).then(() => {
        loaderView.hide();
      });

      // initialize global event handler
      $(window).on("beforeunload", function() {
        if (! model.saveStatus.isSaved()) {
          // last second save
          console.log("Document is unsaved yet. Saving...");
          var resource = ctrl.getPollenConfig("resource");
          var text = editorView.editor.getValue();
          var request = model.saveRequest(text, resource);
          $.ajax({
            method: "POST",
            url: server_api,
            data: request,
            async: false
          });
        }
      });
    },

    save : function() {
      return new Promise((resolve, reject) => {
        var editor = editorView.editor;
        if (editor.isClean(editor.curGen)) {
          resolve();
        }
        editor.curGen = editor.changeGeneration();
        saveStatusView.update(model.saveStatus.saving());
        var resource = this.getPollenConfig("resource");
        var text = editor.getValue();
        rpc.call_server("save", text, resource).then(v => {
          saveStatusView.update(model.saveStatus.saved());
          resolve();
        }).catch(err => {
          saveStatusView.error(err.error);
          reject();
        });
      });
    },

    renderPreview : function() {
      var resource = this.getPollenConfig("resource");
      var request = model.renderRequest(resource);
      var renderedResource = this.getPollenConfig("rendered-resource");
      preview.showLoader();
      model.renderOnTheFly += 1;
      $.post(server_api, request, function(status) {
        preview.reload(renderedResource);
      }).fail(function(status) {
        notifyView.error("Preview reload failed.");
      }).always(function() {
        model.renderOnTheFly -= 1;
        if (model.renderOnTheFly == 0)
          preview.hideLoader();
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
    },

    getRockConfig : function(name) {
      return model.rockConfig[name];
    },

    selectTheme: function(newTheme) {
      model.preference.theme = newTheme;
      if (! newTheme) {
        console.log("p-theme is not found in preference.");
      }
      editorView.selectTheme();
      upperWrapperView.updateBackground();
    },

    getPreference: function() {
      return model.preference;
    }
  };

  //////////////////////////
  // Start Defining Views //
  //////////////////////////
  var editorView = {
    init : function(getPollenConfig) {
      this.view = $("#editor-wrapper");
      var area = document.getElementById("compose");
      this.editor = CodeMirror.fromTextArea(area, ctrl.getPreference());
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
      var scheduledSave;
      // on change event
      this.editor.on("change", function(obj) {
        saveStatusView.update(model.saveStatus.clean());
        // clean scheduled save task
        if (scheduledSave) {
          clearTimeout(scheduledSave);
        }
        // save the document 2 seconds after typing
        // and render the document only when the syntax is correct
        scheduledSave = setTimeout(function() {
          ctrl.save().then(() => {
            if (preview.visible && editorView.syntaxCheck()) {
              ctrl.renderPreview();
            }
          }).catch(() => {
            notifyView.error("Failed to save");
          });
        }, 2000);
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
    },

    // TODO: editor mode should return such a check function
    syntaxCheck : function() {
      var lastLine = this.editor.doc.lastLine();
      var state = this.editor.getStateAfter(lastLine, true);
      return state.braceStack.length == 0;
    },

    selectTheme: function() {
      var p = ctrl.getPreference();
      this.editor.setOption("theme", p.theme);
    }
  };

  var upperWrapperView = {
    // taking the wrapper to indicate init dependencies
    init: function(editorWrapperElement) {
      this.view = $("#upper-wrapper");
      this.wrapperElement = editorWrapperElement;
      this.updateBackground();
    },
    updateBackground: function() {
      var bgColor = $(this.wrapperElement).css('background-color');
      this.view.css('background-color', bgColor);
    }
  };

  var saveStatusView = {
    init : function(status) {
      this.view = $("#savestatus");
      this.update(status);
    },

    update: function(status) {
      this.view.text(status);
    },

    error : function(text) {
      notifyView.error("Failed to save: " + text);
    }

  };

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
      if (! this.visible) {
        this.frame.attr('src', '');
      }
    },

    showLoader: function() {
      this.loader.css('visibility', 'visible');
    },

    hideLoader: function() {
      this.loader.css('visibility', 'hidden');
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
      $(".modal").modal({
        ready: function() {
          $("#tab-header li:first-child a").click();
        }
      });
      $('select').material_select();
    }
  };

  var panelView = {
    init: function(noshell) {
      this.panel = $("#editor-panel");
      shellView.init(noshell);
    }
  };

  var shellView = {
    init : function(noshell) {
      this.view = $("#shell-wrapper");
      this.view.addClass("hide");
      if (noshell) {
        return;
      }
      this.outputTemplate = $('script[data-template="shell-output"]').html();
      this.outputWrapper = $('#shell-output-wrapper');

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

  var preferenceView = {
    init: function() {
      this.preference = ctrl.getPreference();
      this.initThemes();
    },
    initThemes: function() {
      // pre-select current theme
      var curTheme = this.preference.theme;
      $("#p-theme").val(curTheme);
      $('#p-theme').material_select(); // update again!

      $("#p-theme").on("change", function(e) {
        ctrl.selectTheme($(this).val());
      });
    }
  };

  ctrl.init();

});
