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

  var settings = getSettings();
  var area = document.getElementById('input');
  $editor = CodeMirror.fromTextArea(area, settings);

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
  // Elm calls to load rendered result
  app.ports.liveView.subscribe(function(path) {
    console.log('liveView ' + path);
    var frame = document.getElementById('liveViewFrame');
    frame.src = path;
  });

  $editor.on('cursorActivity', function() {
    app.ports.token.send('cursor!');
  });
}

function initLayout() {
  var element = document.getElementById('liveView');
  var resizer = document.getElementById('resizer');
  resizer.addEventListener('mousedown', initResize, false);
  resizer.style.cursor = 'ns-resize';

  //Window funtion mousemove & mouseup
  function initResize(e) {
    console.log("initResize");
    window.addEventListener('mousemove', Resize, false);
    window.addEventListener('mouseup', stopResize, false);
  }
  //resize the element
  function Resize(e) {
    // element.style.width = (e.clientX - element.offsetLeft) + 'px';
    var h = (document.documentElement.clientHeight - e.clientY) + 'px';
    element.style.height = h;
  }
  //on mouseup remove windows functions mousemove & mouseup
  function stopResize(e) {
    console.log("stopResize");
    window.removeEventListener('mousemove', Resize, false);
    window.removeEventListener('mouseup', stopResize, false);
  }
}


initEditor();
initLayout();
