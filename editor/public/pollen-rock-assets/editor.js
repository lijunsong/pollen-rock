'use strict';

var $editor;

// get the file path from URL (strip the first path element that's for
// resource dispatching purpose)
function getFilePath() {
  var pathname = window.location.pathname;
  var path = pathname.replace(/^\/[^\/]+\//, '');
  return path;
}


function initEditor() {
  var $div = document.getElementById('control');
  var app = Elm.Editor.embed($div, {
    filePath: getFilePath()
  });

  var area = document.getElementById('input');
  $editor = CodeMirror.fromTextArea(area, {
    lineNumbers: true
  });

  // -- setup --
  // Elm calls initDoc to set CM content
  app.ports.initDoc.subscribe(function(text) {
    $editor.setValue(text);
    $editor.refresh();
  });
  // Elm calls askCMContent to get CM content
  app.ports.askCMContent.subscribe(function() {
    var contents = $editor.getValue();
    app.ports.getCMContent.send(contents);
  });
  // notify Elm on data changed
  var docGeneration = null;
  $editor.on('change', function() {
    if (! $editor.isClean(docGeneration)) {
      docGeneration = $editor.changeGeneration();
      app.ports.markContentsDirty.send(docGeneration);
    }
  });

  $editor.on('cursorActivity', function() {
    app.ports.token.send('cursor!');
  });
}


initEditor();
