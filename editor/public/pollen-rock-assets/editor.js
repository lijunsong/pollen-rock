'use strict';

// notification
var notify = new Notify();

// codemirror instance
var $editor;

// pollenSetup that the editor is going to use
var pollenSetup = {
  commandChar: '@'
};


// get the file path from URL (strip the first path element that's for
// resource dispatching purpose)
function getFilePath() {
  var pathname = window.location.pathname;
  var path = pathname.replace(/^\/[^\/]+\//, '');
  return path;
}

// code mirror handler. TODO: read commandChar from /rest/config/$path.
function  insertCommandCharHandler(e) {
  let pos = e.getCursor();
  let commandChar = pollenSetup.commandChar;
  if (pos.ch == 0) {
    e.replaceSelection(commandChar);
  } else {
    let lastPos = CodeMirror.Pos(pos.line, pos.ch-1);
    if (e.getRange(lastPos, pos) == commandChar) {
      e.replaceRange('@', lastPos, pos);
    } else {
      e.replaceSelection(commandChar);
    }
  }
}

function initEditor() {
  var $div = document.getElementById('control');
  var app = Elm.Editor.embed($div, {
    filePath: getFilePath()
  });

  var settings = getSettings();
  var area = document.getElementById('input');
  $editor = CodeMirror.fromTextArea(area, settings);

  // -- Elm setup --
  // Elm calls info to pop up info message
  app.ports.notifyInfo.subscribe(function(arg) {
    var [msg, time] = arg;
    notify.info(msg, time);
  });
  app.ports.notifyError.subscribe(function(arg) {
    var [msg, time] = arg;
    notify.error(msg, time);
  });
  // Elm calls initDoc to set CM content
  app.ports.initDoc.subscribe(function(text) {
    $editor.setValue(text);
    $editor.refresh();
    $editor.focus();
  });
  // Elm calls CodeMirror to change option
  app.ports.setCMOption.subscribe(function(arg) {
    var [option, value] = arg;
    console.log(`set ${option} to ${value}`);
    $editor.setOption(option, value);
    $editor.refresh();
  });
  // Elm calls askCMContent to get CM content
  app.ports.askCMContent.subscribe(function() {
    var contents = $editor.getValue();
    var generation = $editor.changeGeneration();
    var mode = $editor.doc.getMode();
    var checkFunc = CodeMirror.syntaxCheck[mode.name];
    var checkResult = true; // default to true

    if (checkFunc) {
      checkResult = checkFunc($editor, $editor.doc.lastLine());
      console.log('syntax check: ' + checkResult);
    }

    app.ports.getCMContent.send({
      contents: contents,
      generation: generation,
      syntaxCheckResult: checkResult
    });
  });
  // Elm calls allowClose to let JS know if it should block
  // closing tab to prevent unsaved data loss.
  var allowClose = true;
  app.ports.allowClose.subscribe(function(allow) {
    allowClose = allow;
  });
  // notify Elm on data changed, and make allowClose false
  // to prevent unsaved data loss.
  $editor.on('change', function() {
    // args of markContentsDirty is not used
    app.ports.markContentsDirty.send(0);
  });
  // Elm calls to load rendered result
  app.ports.liveView.subscribe(function(path) {
    console.log('liveView ' + path);
    var frame = document.getElementById('liveViewFrame');
    if (! frame.src || frame.contentWindow.location.pathname != path) {
      frame.src = path;
    } else {
      frame.contentWindow.location.reload();
    }
  });
  // Elm calls to switch layout
  app.ports.changeLayout.subscribe(function(layout) {
    console.log('set layout to be ' + layout);
    $renderPanel.setLayout(layout);
    $editor.focus();
  });
  // Elm calls to update the pollenSetup defined in the project
  app.ports.updatePollenSetup.subscribe(function(setup) {
    console.log(setup);
    pollenSetup = setup;
  });

  window.addEventListener('beforeunload', (e) => {
    if (allowClose == false) {
      e.returnValue = "Saving your contens";
      return e.returnValue;
    }
    return undefined;
  });

  // -- key map setup --
  $editor.setOption("extraKeys", {
    "'@'": insertCommandCharHandler
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
class RenderPanel {
  constructor() {
    // DOM bind
    this.liveView = document.getElementById('liveView');
    this.liveViewFrame = document.getElementById('liveViewFrame');
    this.resizer = document.getElementById('resizer');
    this.listener = document.getElementById('listener');

    // create reusable methods for attach, remove event listener
    this.startDragging = this.__startDragging.bind(this);
    this.stopDragging = this.__stopDragging.bind(this);
    this.resize = this.__resize.bind(this);

    // attach event only on resizer
    this.resizer.addEventListener('mousedown', this.startDragging, false);
    this.setLayout('close');
  }

  setLayout(layout) {
    this.layout = layout;
    this.__updateLayoutView();
  }


  __startDragging() {
    this.listener.style['z-index'] = 10;
    window.addEventListener('mousemove', this.resize, false);
    window.addEventListener('mouseup', this.stopDragging, false);
  }

  __stopDragging() {
    this.listener.style['z-index'] = -1;
    window.removeEventListener('mousemove', this.resize, false);
    window.removeEventListener('mouseup', this.stopDragging, false);
    $editor.refresh();
  }

  __resize(e) {
    if (this.layout == 'close') {
      // shouldn't happen, but let's just return immediately anyway
      return;
    } else if (this.layout == 'horizontal') {
      var h = (document.documentElement.clientHeight - e.clientY) + 'px';
      this.liveView.style.height = h;
    } else if (this.layout == 'vertical') {
      var w = (document.documentElement.clientWidth - e.clientX) + 'px';
      this.liveView.style.width = w;
    } else {
      console.log('[resize] unknown layout: ' + this.layout);
    }
  }

  __updateLayoutView() {

    if (this.layout == 'close') {
      this.liveView.classList.add('hidden');
    } else if (this.layout == 'horizontal' || this.layout == 'vertical') {
      this.liveView.classList.remove('hidden');
      var container = document.getElementById('container');
      var resizerStyle = this.resizer.style;
      if (this.layout == 'horizontal') {
        this.liveView.style.width = '100%';
        this.liveView.style.height = '50%';
        container.style['flex-flow'] = 'column';
        resizerStyle.cursor = 'row-resize';
        resizerStyle.position = 'sticky';
        resizerStyle.height = '8px';
        resizerStyle.width = '100%';
        resizerStyle.top = '0';
        resizerStyle.left = '0';
        resizerStyle.right = '0';
        resizerStyle.bottom = null;
      } else {
        this.liveView.style.height = '100%';
        this.liveView.style.width = '50%';
        container.style['flex-flow'] = 'row';
        resizerStyle.cursor = 'col-resize';
        resizerStyle.position = 'absolute';
        resizerStyle.height = '100%';
        resizerStyle.width = '8px';
        resizerStyle.top = '0';
        resizerStyle.left = '0';
        resizerStyle.right = null;
        resizerStyle.bottom = '0';
      }
    } else {
      console.log('[updateLayout] unknown layout: ' + this.layout);
    }

	if ($editor) {
		$editor.refresh();
	}
  }

}

// When new document loaded in iframe and the target is one pollen
// source file, pollen-rock needs to redirect the editor to a new
// source.
function initFrame() {
  var $frame = document.getElementById('liveViewFrame');
  $frame.addEventListener('load', function() {
    var editingPath = window.location.pathname;
    var goingPath = $frame.contentWindow.location.pathname;

    if (editingPath.replace('/editor', '').startsWith(goingPath)) {
      return;
    }
    // redirect to the source that can produce goingPath
    var xmlHttp = new XMLHttpRequest();
    xmlHttp.onreadystatechange = function() {
      if (xmlHttp.readyState == 4 && xmlHttp.status == 200) {
        let response = JSON.parse(xmlHttp.responseText);
        if (response.errno != 0) {
          console.log(`${goingPath} is not found on filesystem`);
        } else if (! editingPath.endsWith(response.source)){
          notify.info(`Click <a href="/editor/${response.source}">here</a> to edit ${response.source}.`, 5000);
        }
      }
    };
    xmlHttp.open('GET', `/rest/search${goingPath}`, true);
    xmlHttp.send(null);
  });
}

var $renderPanel = new RenderPanel();
initEditor();
initFrame();
