#lang racket

(require web-server/http/request-structs)
(require json)
(require "util.rkt")
(require "http-util.rkt")
(require "logger.rkt")

(provide (prefix-out restful- (all-defined-out)))

;; To add more handlers for different route, add restful route in
;; main-handler, and define a sub-handler accepting request and
;; matched-url (path elements)
;;
;; NOTE: other handlers must return a Jsexpr


;; main handler dispatch request to different handlers.
(define (main-handler req type matched-url)
  (print-request req)
  (define ans
    (match type
      ["fs" (fs-handler req matched-url)]))
  ;;; convert ans to jsexp
  (response/text (jsexpr->bytes ans)))

(define/contract (extract-bindings key bindings)
  (-> bytes? (listof binding?) (or/c false? bytes?))
  (let [(form (bindings-assq key bindings))]
    (if form
        (binding:form-value form)
        false)))


;;;; POST /fs/$path

(define/contract (fs-answer errno [message (json-null)])
  (->* (integer?) (jsexpr?) jsexpr?)
  (hasheq 'errno errno
          'message message))

(define/contract (mv-op src dst)
  (-> string? string? jsexpr?)
  (cond [(and (non-empty-string? dst) (non-empty-string? src))
         (log-rest-debug "mv ~s ~s" src dst)
         (fs-answer 0)]
        [else
         (fs-answer 1 (format "mv failed: mv ~s ~s" src dst))]))

(define (fs-handler req matched-url)
  ;; mv op
  (define bindings (request-bindings/raw req))
  (match (extract-bindings #"op" bindings)
    [#"mv"
     (let [(src (path-elements->resource matched-url))
           (dst (extract-bindings #"data" bindings))]
       (mv-op src
              (if dst
                  (bytes->string/utf-8 dst) "")))]
    [else
     (fs-answer 1 "op field doesn't exist")]))
