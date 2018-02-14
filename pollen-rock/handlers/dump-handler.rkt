#lang racket

(require "../http-util.rkt")
(require "../logger.rkt")
(require web-server/http)

(provide (all-defined-out))

(define (dump-handler req url-parts)
  (print-request req)
  (log-rest-debug "url: ~s" url-parts)
  "from dump-handler")
