$(function() {
  var server_api = "/api";

  var model = {
    init : function() {

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
    pollenConfigRequest : function() {
      return {
        type : 'pollenConfig'
      };
    }
  };

  ////////////////////////////////////////////////////
  // ctrl is the bridge between the model and views //
  ////////////////////////////////////////////////////
  var ctrl = {
    init : function() {
      model.init();
      editorview.init();
      preview.init();
      saveStatusView.init();

      this.initPollenConfig();
      setInterval(function() {
        this.save();
      }, 2000);
    },

    initPollenConfig : function() {
      this.pollenConfig = {};
      // TODO: send request to server
      this.pollenConfig["commandChar"] = "◊";
    },

    save : function() {
      var editor = editorview.editor;
      if (editor.isClean(editor.curGen)) {
        return;
      }
      editor.curGen = editor.changeGeneration();
      saveStatusView.saving();
      var resource = editor.resource;
      var text = editor.getValue();
      var request = model.saveRequest(text, resource);
      $.post(server_api, request, function(status) {
        saveStatusView.saved();
      }).fail(function(status) {
        saveStatusView.error();
      });
    },

    render : function() {
      var resource = this.pollenConfig["resource"];
      var request = model.renderRequest(resource);
      $.post(server_api, request, function(status) {
        preview.reload(resource);
      }).fail(function(status) {
        $.notify("server error");
      });
    }
  };

  ////////////////////////////
  // create editor instance //
  ////////////////////////////
  var editorview = {
    init : function() {
      var area = $("#editor");
      this.editor = CodeMirror.fromTextArea(area, {
        autofocus: true,
        matchBrackets: true,
        lineWrapper: true,
        mode: 'pollen',
        lineNumbers: 'true'
      });
      this.fullscreen = this.initFullscreen();
      this.initKeyMaps();

      return this.editor;
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
      function defineKey(key, func, doc) {
        return {
          key: key,
          doc: doc,
          func: func
        };
      }

      var keyMaps = [
        defineKey('Shift-2', function(e) {
          var pos = e.getCursor();
          if (pos.ch == 0)
            e.replaceSelection(this.editor.commandChar);
          else {
            var lastPos = CodeMirror.Pos(pos.line, pos.ch-1);
            if (e.getRange(lastPos, pos) == this.editor.commandChar)
              e.replaceRange('@', lastPos, pos);
            else
              e.replaceSelection(this.editor.commandChar);
          }
        }, "Insert Command Char or @"),

        defineKey('Cmd-Enter', function() {
          if (this.fullscreen) {
            this.fullscreen();
          } else {
            $.notify("Your Browser Doesn't Support Fullscreen");
          }
        }, "Enter Fullscreen"),

        defineKey('Shift-Ctrl-P', function() {
          preview.toggleHide();
          if (preview.visible) {
            this.toRightView();
          } else {
            this.toCenterView();
          }
        }, "Open/Close Preview Window")
      ];

      for (var i = 0; i < keyMaps.length; i++) {
        var m = {};
        m[keyMaps[i].key] = keyMaps[i].func;
        this.editor.addKeyMap(m);
      }
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
      this.dom = $("#view");
      if (! this.dom.addClass("hide"))
        this.dom.addClass("hide");
      this.visible = this.dom.hasClass("hide");
    },

    toggleHide: function() {
      this.dom.toggleClass("hide");
      this.visible = this.dom.hasClass("hide");
    },

    reload: function(src) {
      this.dom.attr('src', src);
    }
  };

}());
