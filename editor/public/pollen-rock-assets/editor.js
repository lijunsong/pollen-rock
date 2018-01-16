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

/* The problem is that mouse event won't pop up to the iframe's parent
   when the mouse moves into iframe. So dragging stops when our mouse
   moves into iframe.

   The trick here is to have another canvas `listener` over the iframe
   when we starts dragging. Listener is not part of iframe, so it can
   listen our mouse event. When dragging stops, Listener goes behind
   iframe so it won't interfere with the mouse scroll event on iframe.
 */
function initLayout() {
  var liveView = document.getElementById('liveView');
  var liveViewFrame = document.getElementById('liveViewFrame');
  var resizer = document.getElementById('resizer');
  var listener = document.getElementById('listener');
  resizer.addEventListener('mousedown', initResize, false);
  resizer.style.cursor = 'ns-resize';

  //Window funtion mousemove & mouseup. We keep listener above iframe
  function initResize(e) {
    console.log("initResize");
    listener.style['z-index'] = 10;
    window.addEventListener('mousemove', Resize, false);
    window.addEventListener('mouseup', stopResize, false);
  }
  //resize the liveView
  function Resize(e) {
    // liveView.style.width = (e.clientX - liveView.offsetLeft) + 'px';
    var h = (document.documentElement.clientHeight - e.clientY) + 'px';
    liveView.style.height = h;
    liveViewFrame.style.height = h;
  }
  //on mouseup remove windows functions mousemove & mouseup. We keep
  // listener behind iframe
  function stopResize(e) {
    console.log("stopResize");
    listener.style['z-index'] = -1;
    window.removeEventListener('mousemove', Resize, false);
    window.removeEventListener('mouseup', stopResize, false);
  }
}


initEditor();
initLayout();
