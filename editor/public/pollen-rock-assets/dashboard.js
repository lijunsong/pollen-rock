'use strict';

var settings = getSettings();
var app = Elm.Dashboard.fullscreen(settings);
app.ports.setStringSettings.subscribe(setSettingsItem);
app.ports.setNumberSettings.subscribe(setSettingsItem);
app.ports.setBoolSettings.subscribe(setSettingsItem);
app.ports.resetSettings.subscribe(resetSettings);
