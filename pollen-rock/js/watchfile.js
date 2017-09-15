$(document).ready(function() {
  "use strict";

  var MAXRETRY = 10;
  var rpc = new PollenRockRPC("/api");

  var model = {
    init: function() {
      this.lastSeenSeconds = 0;
      this.resource = $("#autorender-data").attr("data");
      this.retry = 0;
    }
  };

  var ctrl = {
    init: function() {
      model.init();
      deadView.init();
      loaderView.init();
      autorender.init();
      notifyView.info("Watching " + model.resource);
    },

    watch: function() {
      rpc.call_server("watchfile", model.resource, model.lastSeenSeconds||0).then(rpcVal => {
        let result = rpcVal.result;
        if (! result["rendered-resource"] || result.seconds == 0) {
          notifyView.info("corrupted response. Continue to connect...");
        }
        model.lastSeenSeconds = result.seconds;
        autorender.reload(result["rendered-resource"]);
        model.retry = 0;
        ctrl.watch();
      }).catch(rpcVal => {
        // depends on file system changes, the request may be sent out
        // in the middle of an replace operation, retry a few times if
        // failed.
        model.retry += 1;
        if (model.retry <= MAXRETRY) {
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
      this.view = $("#autorender-loader");
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

  var autorender = {
    init: function() {
      this.frame = $("#autorender-frame");
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
