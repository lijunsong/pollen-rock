$(document).ready(function() {
  "use strict";

  // 0 will be used as known last modification seconds when seconds
  // is not passed in.
  function watchFileRequest(resource, seconds) {
    return {
      type : 'watchfile',
      resource : resource,
      seconds : seconds || 0
    };
  }

  var resource = $("#preview-data").attr("data");
  var lastSeenSeconds = 0;
  var frame = $("#preview-frame");

  function watch() {
    var wfRequest = watchFileRequest(resource, lastSeenSeconds);
    $.post("/api", wfRequest, function(res){
      var result = JSON.parse(res);
      if (! result["rendered-resource"] || result.seconds == 0) {
        console.log("corrupted response.");
      }
      lastSeenSeconds = result.seconds;
      if (frame.attr('src')) {
        var doc = frame.contents()[0];
        doc.location.reload(true);
      } else {
        frame.attr('src', result["rendered-resource"]);
      }
      watch();
    }).fail(function() {
      console.log("failed. continue");
      watch();
    }).always(function() {
    });
  }

  watch();
});
