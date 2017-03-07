$(document).ready(function() {
  "use strict";

  var serverAPI = "/api";

  // 0 will be used as known last modification seconds when seconds
  // is not passed in.
  function watchFileRequest(resource, seconds) {
    return {
      type : 'watchfile',
      resource : resource,
      seconds : seconds || 0
    };
  }

  var model = {
    init: function() {
      this.lastSeenSeconds = 0;
      this.resource = $("#preview-data").attr("data");
      this.retry = 0;
    }
  };

  var ctrl = {
    init: function() {
      model.init();
      deadView.init();
      loaderView.init();
      preview.init();
      notifyView.info("Watching " + model.resource);
    },

    watch: function() {
      var wfRequest = watchFileRequest(model.resource,
                                       model.lastSeenSeconds);
      $.post(serverAPI, wfRequest, function(res){
        var result = JSON.parse(res);
        if (! result["rendered-resource"] || result.seconds == 0) {
          notifyView.info("corrupted response. Continue to connect...");
        }
        model.lastSeenSeconds = result.seconds;
        preview.reload(result["rendered-resource"]);
        model.retry = 0;
        ctrl.watch();
      }).fail(function(status) {
        // depends on file system changes, the request may be sent out
        // in the middle of an replace operation, retry a few times if
        // failed.
        model.retry += 1;
        if (model.retry <= 10) {
          loaderView.show();
          setTimeout(function() {
            loaderView.hide();
            ctrl.watch();
          }, 1000);
        } else {
          notifyView.error("Server error. Stop connecting.");
          loaderView.hide();
          deadView.show();
        }
      });
    }
  };

  var loaderView = {
    init: function() {
      this.view = $("#preview-loader");
      this.hide();
    },
    show: function() {
      this.view.show();
    },
    hide: function() {
      this.view.hide();
    }
  };

  var notifyView = {
    info : function(msg) {
      Materialize.toast(msg, 4000, 'toast-info');
    },
    error : function(msg) {
      console.log("error: "+msg);
      Materialize.toast(msg, 4000, 'toast-error');
    }
  };

  var preview = {
    init: function() {
      this.frame = $("#preview-frame");
    },
    reload: function(src) {
      loaderView.show();
      this.frame.on('load', function(){
        loaderView.hide();
      });
      if (this.frame.attr('src')) {
        var doc = this.frame.contents()[0];
        doc.location.reload(true);
      } else {
        this.frame.attr('src', src);
      }
    }
  };

  var deadView = {
    init: function() {
      this.view = $("#dead");
      this.view.hide();
    },
    show: function() {
      this.view.show();
    }
  };

  ctrl.init();

  ctrl.watch();
});
