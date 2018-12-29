const proxy = require('http-proxy-middleware');

/// Have this proxy service to redirect all requests to a different
/// port so iframe's safe-origin policy won't affect development
module.exports = function(app) {
  console.log(app);
  app.use(proxy("/devremote", {
    target: 'http://localhost:8000',
    pathRewrite: {
      "^/devremote": "/"
    },
    logLevel: 'debug',
  }));
};
