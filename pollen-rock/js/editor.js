"use strict";

$(document).ready(() => {
  let resource = $("#compose").attr("data");
  let $textarea = document.getElementById("compose");
  let model = new Model($textarea, resource);
  let view = new View(model);
  let controller = new Controller(model, view);
  // hand off the rest initialization to controller constructor
});
