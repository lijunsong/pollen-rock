'use strict';

var $pollenEditor;


function initEditor() {
  var area = document.getElementById('input');
  $pollenEditor = CodeMirror.fromTextArea(area, {
    lineNumbers: true
  });

  var $div = document.getElementById('control');
  var app = Elm.Editor.embed($div);
  $pollenEditor.on('cursorActivity', function() {
    app.ports.token.send('cursor!');
  });
}


initEditor();
