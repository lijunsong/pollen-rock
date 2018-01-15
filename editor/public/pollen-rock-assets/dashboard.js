'use strict';

var settings = getSettings();
var app = Elm.Dashboard.fullscreen(settings);
app.ports.setSettings.subscribe(function(settings) {
  setSettings(settings);
});
