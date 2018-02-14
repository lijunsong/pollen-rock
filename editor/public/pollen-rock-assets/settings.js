'use strict';

var pollenRockSettingsName = 'pollen-rock-settings';

function emptySettings() {
  return {
    lineNumbers: false,
    lineWrapping: true
  };
}

function setSettings(settings) {
  var storage = window.localStorage;
  storage.setItem(pollenRockSettingsName, JSON.stringify(settings));
}


function setSettingsItem(args) {
  console.log(`update ${args}`);
  var settings = getSettings();
  var [name, val] = args;
  settings[name] = val;
  setSettings(settings);
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

function resetSettings() {
  window.localStorage.clear();
}
