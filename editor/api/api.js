'use strict';

const jsonServer = require('json-server');
const path = require('path');

const server = jsonServer.create();
const router = jsonServer.router(path.join(__dirname, 'db.json'));
const middlewares = jsonServer.router('db.json');
const port = 4000;

server.use(middlewares);

server.use(jsonServer.bodyParser);

// Server would return a list of objects instead of a singular when
// rewrite the query to using param. Make server respond to singular=1
// query param to avoid it.
server.use((req, res, ext) => {
  const _send = res.send;
  res.send = function (body) {
    if (require('url').parse(req.originalUrl, true).query['singular']) {
      try {
        const json = JSON.parse(body);
        if (Array.isArray(json)) {
          if (json.length === 1) {
            return _send.call(this, JSON.stringify(json[0]));
          } else if (json.length === 0) {
            return _send.call(this, '{}', 404);
          }
        }
      } catch (e) {}
    }
    return _send.call(this, body);
  };
  next();
});

// rewrite route
server.use(jsonServer.rewriter({
  '/rest/*': '/$1',
  '/folder/*': '/folder?path=$1'
}));

server.use(router);

server.listen(port, () => {
  console.log(`JSON Server is running at port ${port}`);
});
