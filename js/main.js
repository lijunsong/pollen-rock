function save(editor) {
  if (editor.isClean(editor.curGen)) {
    return;
  }
  editor.curGen = editor.changeGeneration();
  $("#savestatus").text('saving');
  var text = editor.getValue();
  var file = $("#editor").attr("data");
  // TODO: since server is stateless, post won't know whether server
  // has saved the file correctly.
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

$(document).ready(function () {
  var area = document.getElementById("compose");

  var pollenEditor = CodeMirror.fromTextArea(area, {
    autofocus: true,
    mode: 'pollen',
    lineWrapping: true,
    lineNumbers: true,
    matchBrackets: true
  });

  pollenEditor.addKeyMap({
    // shift-2 is char @
    'Shift-2': function(cm) {
      cm.replaceSelection('◊');
    }
  });

  pollenEditor.on("change", function(obj) {
    $("#savestatus").empty();
  });
  /*
    CodeMirror.on(pollenEditor, 'keydown', function(cm, event) {
    console.log(event.keyCode);
    });*/

  setInterval(function() { save(pollenEditor); }, 2000);
});
