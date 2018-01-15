'use strict';

var pollenRockSettingsName = 'pollen-rock-settings';

function emptySettings() {
  return {
    lineNumbers: false,
    lineWrapping: true,
    font: 'Source Sans Pro'
  };
}

function setSettings(settings) {
  var storage = window.localStorage;
  storage.setItem(pollenRockSettingsName, JSON.stringify(settings));
}

function getSettings() {
  var storage = window.localStorage;

  var settingsString = storage.getItem(pollenRockSettingsName);
  var settings = null;

  if (! settingsString) {
    settings = emptySettings();
    setSettings(settings);
  } else {
    settings = JSON.parse(settingsString);
  }
  return settings;
}
