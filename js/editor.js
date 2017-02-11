$(function() {
  "use strict";

  var server_api = "/api";

  var model = {
    initPollenConfig : function(config) {
      this["pollenConfig"] = config;
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

      $.post(server_api, initRequest, function(config) {
        // continue init
        var jsconfig = JSON.parse(config);
        jsconfig["resource"] = resource;
        ctrl.initRest(jsconfig);
        $.notify("Ready to Rock.");
      }).fail(function(status) {
        $.notify(status.statusText);
      });
    },

    initRest : function(jsconfig) {
      model.initPollenConfig(jsconfig);
      editorview.init();
      preview.init();
      saveStatusView.init();

      setInterval(function() {
        ctrl.save();
      }, 2000);
    },

    save : function() {
      var editor = editorview.editor;
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
        $.notify("server error");
      });
    },

    getPollenConfig : function(id) {
      return model.pollenConfig[id];
    }
  };

  ////////////////////////////
  // create editor instance //
  ////////////////////////////
  var editorview = {
    init : function() {
      this.view = $("#editorwrapper");
      var area = document.getElementById("compose");
      this.editor = CodeMirror.fromTextArea(area, {
        autofocus: true,
        matchBrackets: true,
        lineWrapper: true,
        mode: 'pollen',
        lineNumbers: 'true'
      });
      this.fullscreen = this.initFullscreen();
      this.initKeyMaps();
      this.initEditorStyle();

      return this.editor;
    },

    initEditorStyle : function() {
      var width = this.editor.defaultCharWidth()*80;
      this.view.css("max-width", width+'px');
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

    initKeyMaps: function() {
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

      var commandChar = ctrl.getPollenConfig("command-char");
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
            $.notify("Your Browser Doesn't Support Fullscreen");
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
    },

    toRight : function() {
      this.view.addClass("side-right");
    },

    toCenter : function() {
      this.view.removeClass("side-right");
    }
  };

  var saveStatusView = {
    init : function() {
      this.view = $("#savestatus");
    },

    saved : function() {
      this.view.text("saved");
    },

    saving : function() {
      this.view.text("saving");
    },

    empty : function() {
      this.view.empty();
    },

    error : function(text) {
      $.notify(text);
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
      this.frame.attr('src', src);
    }
  };

  ctrl.init();
}());
