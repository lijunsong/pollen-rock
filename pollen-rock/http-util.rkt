#lang racket

(require "util.rkt")
(require web-server/http/request-structs)  ;request
(require web-server/http/response-structs) ;response
(require net/url-structs)                  ;url

(provide (all-defined-out))

;; extract resource from a request
(define/contract (request->resource req)
  (-> request? resource?)
  (define uri (request-uri req))
  (define string-list (map path/param-path (url-path uri)))
  (path-elements->resource string-list))

;; construct a response contains only text
;; example: (response/text "1" "2" "3")
(define/contract (response/text . text)
  (->* () () #:rest (listof (or/c bytes? string?)) response?)
  (define (->bytes x)
    (if (string? x) (string->bytes/utf-8 x) x))
  (define/contract ans (listof bytes?) (map ->bytes text))
  (response/full
   200 #"Okay" (current-seconds)
   TEXT/HTML-MIME-TYPE
   empty
   ans))
