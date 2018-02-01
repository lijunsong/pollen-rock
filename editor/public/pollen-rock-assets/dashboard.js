'use strict';

var settings = getSettings();
var app = Elm.Dashboard.fullscreen(settings);
app.ports.setStringSettings.subscribe(setSettingsItem);
app.ports.setNumberSettings.subscribe(setSettingsItem);
app.ports.setBoolSettings.subscribe(setSettingsItem);
app.ports.resetSettings.subscribe(resetSettings);
app.ports.reloadRenderFrame.subscribe(function(loc) {
  var frame = document.getElementById('renderFrame');
  if (! frame.src) {
    frame.src = loc;
  } else {
    frame.contentWindow.location.reload();
  }
});
