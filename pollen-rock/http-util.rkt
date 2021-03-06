#lang racket

(require web-server/http/request-structs)  ; request
(require web-server/http/response-structs) ; response
(require net/url-structs)                  ; url
(require net/url-string)
(require "logger.rkt")                     ; logger

(provide (all-defined-out))


(define/contract (get-binding-value key bindings)
  (-> bytes? (listof binding?) (or/c false? bytes?))
  (let [(form (bindings-assq key bindings))]
    (if form
        (binding:form-value form)
        false)))


(define/contract (make-test-request kind url-parts binding-hash)
  (-> string? (listof string?) (hash/c bytes? bytes?) request?)
  (let [(bindings (hash-map binding-hash
                            (lambda (k v)
                              (make-binding:form k v))))]
    (make-request
     #"POST" (string->url
              (format "http://localhost:8000/rest/~a/~a"
                      kind
                      (relative-path->relative-url-string
                       (apply build-path url-parts))))
     empty
     (delay bindings)
   #"fake post not used"
   "0.0.0.0"
   8000
   "0.0.0.0")))


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
   ;; allow any domain to do anything
   (list (make-header #"Access-Control-Allow-Origin" #"*"))
   ans))

;; Debug use
(define (print-request req)
  (let ((uri (request-uri req)))
     (log-web-request-debug "query: ~a" (url-query uri))
     (log-web-request-debug "path: ~a" (url-path uri))
     (log-web-request-debug "bindings: ~a" (request-bindings/raw req))
     (log-web-request-debug "post-data: ~a" (request-post-data/raw req))))
