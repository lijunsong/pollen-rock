'use strict';

// Server would return a list of objects instead of a singular when
// rewrite the query to using param. Make server respond to singular=1
// query param to avoid it.

module.exports = (req, res, next) => {
  const _send = res.send
  res.send = function (body) {
    console.log(req.url);
    if (require('url').parse(req.url, true).query['singular']) {
      try {
        const json = JSON.parse(body)
        if (Array.isArray(json)) {
          if (json.length === 1) {
            return _send.call(this, JSON.stringify(json[0]))
          } else if (json.length === 0) {
            return _send.call(this, '{}', 404)
          }
        }
      } catch (e) {}
    }
    return _send.call(this, body)
  }
  next()
}
