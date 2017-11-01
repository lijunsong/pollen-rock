#lang racket

(require rackunit)
(require net/url-string)
(require web-server/http/request-structs)
; (require web-server/http/response-structs)
(require "api-util.rkt")


(define/contract (make-test-request kind url-parts binding-hash)
  (-> string? (listof string?) (hash/c bytes? bytes?) request?)
  (let [(bindings (hash-map binding-hash
                            (lambda (k v)
                              (make-binding:form k v))))]
    (make-request
     #"POST" (string->url
              (format "http://localhost:8000/rest/~a~a"
                      kind
                      (path-elements->resource url-parts)))
     empty
     (delay bindings)
   #"fake post not used"
   "0.0.0.0"
   8000
   "0.0.0.0")))
