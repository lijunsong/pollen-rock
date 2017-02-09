function save(editor)
{
  if (editor.isClean(editor.curGen)) {
    return;
  }
  editor.curGen = editor.changeGeneration();
  $("#savestatus").text('saving');
  var text = editor.getValue();
  var file = $("#editor").attr("data");

  var post_data = {
    type : 'autosave',
    text : text,
    resource : file
  };
  $.post("/api", post_data, function(status) {
    $("#savestatus").text(status);
  }).fail(function(status) {
    $("#savestatus").text(status.statusText);
  });
}

function render(editor, resource) {
  if (! resource) {
    resource = $("#editor").attr("data");
  }
  var post_data = {
    type : 'render',
    resource : resource
  };
  $.post("/api", post_data, function(status) {
    $("#preview").attr('src', status);
    //$.notify(status);
  }).fail(function(status) {
    $.notify(status.statusText);
  });
}

// return a fullscreen function or false
function initFullscreen()
{
  var fscreen = false;
  var f = ["requestFullscreen", "mozRequestFullScreen",
           "webkitRequestFullscreen", "msRequestFullscreen"];
  for (var i = 0; i < f.length; i++) {
    if (f[i] in document.documentElement) {
      return function() {document.documentElement[f[i]]();};
    }
  }
  return false;
}

// setup notify styling
function initNotify()
{
  $.notify.addStyle('default', {
    html: "<span data-notify-text/>",
    classes: {
      base: {
        "font-family": "'source code pro', ubuntu",
        "background-color": "#f4fa00",
        "padding": "5px"
      }
    }
  });
  $.notify.defaults({
    style: 'default',
    className: 'notify'
  });
}

// setup editor
function initEditor(id)
{
  // setup editor
  var area = document.getElementById(id);

  var pollenEditor = CodeMirror.fromTextArea(area, {
    autofocus: true,
    mode: 'pollen',
    lineWrapping: true,
    lineNumbers: true,
    matchBrackets: true
  });

  pollenEditor.commandChar = '◊';

  window.eee = pollenEditor;
  return pollenEditor;
}

// define and setup editor's key map
function initEditorKey(editor)
{
  // return an object containing doc that be used to
  // build up keyMap hint later.
  function defineKey(key, func, doc) {
    return {
      key: key,
      doc: doc,
      func: func
    };
  }

  // helper function for fullscreen
  var fullscreen = initFullscreen();

  var keyMaps = [
    defineKey('Shift-2', function(e) {
      var pos = e.getCursor();
      if (pos.ch == 0)
        e.replaceSelection(editor.commandChar);
      else {
        var lastPos = CodeMirror.Pos(pos.line, pos.ch-1);
        if (e.getRange(lastPos, pos) == editor.commandChar)
          e.replaceRange('@', lastPos, pos);
        else
          e.replaceSelection(editor.commandChar);
      }
    }, "Insert Command Char or @"),

    defineKey('Cmd-Enter', function() {
      if (fullscreen) {
        fullscreen();
      } else {
        $.notify("Your Browser Doesn't Support Fullscreen");
      }
    }, "Enter Fullscreen"),

    defineKey('Shift-Ctrl-P', function() {
      var leftside = $("#view");
      leftside.toggleClass("hide");
      if (! leftside.hasClass("hide")) {
        $("#container").addClass("side-right");
        render(editor);
      } else {
        $("#container").removeClass("side-right");
      }
    }, "Open/Close Previse Window")
  ];

  for (var i = 0; i < keyMaps.length; i++) {
    var m = {};
    m[keyMaps[i].key] = keyMaps[i].func;
    editor.addKeyMap(m);
  }
}

// setup editor's event handler
function initEditorEventHandler(editor)
{
  editor.on("change", function(obj) {
    $("#savestatus").empty();
  });

  setInterval(function() {
    save(editor);
  }, 2000);
}

function initEditorStyle(editor)
{
  var width = editor.defaultCharWidth()*80;
  $("#container").css("max-width", width+'px');
}

// talk to server to get pollen config for this file
function initEditorPollenConfig(editor)
{

}

$(document).ready(function () {
  // setup nofify position
  initNotify();

  // setup editor
  var pollenEditor = initEditor("compose");
  initEditorKey(pollenEditor);
  initEditorEventHandler(pollenEditor);
  initEditorStyle(pollenEditor);
  initEditorPollenConfig(pollenEditor);

});
