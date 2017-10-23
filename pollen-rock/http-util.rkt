#lang racket

(require "util.rkt")
(require web-server/http/request-structs)  ;request
(require web-server/http/response-structs) ;response
(require net/url-structs)                  ;url
(require "logger.rkt")                     ;logger

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

;;; Debug use
(define (print-request req)
  (let ((uri (request-uri req)))
     (log-web-request-debug "query: ~a" (url-query uri))
     (log-web-request-debug "path: ~a" (url-path uri))
     (log-web-request-debug "bindings: ~a" (request-bindings/raw req))
     (log-web-request-debug "post-data: ~a" (request-post-data/raw req))))
