#lang racket

(require web-server/http/request-structs)
(require json)
(require "http-util.rkt"
         "logger.rkt"
         "handlers/fs-handler.rkt"
         "handlers/get-contents-handler.rkt"
         "handlers/get-config-handler.rkt"
         "handlers/render-handler.rkt"
         "handlers/search-handler.rkt"
         "handlers/tags-handler.rkt"
         "handlers/watch-handler.rkt"
         "handlers/dump-handler.rkt")

(provide (all-defined-out))

;; To add more handlers for different route, add restful route in
;; main-handler, and define a sub-handler accepting request and
;; url-parts (path elements)
;;
;; NOTE: sub handlers must return a Jsexpr


;; main handler dispatch request to different handlers.
(define (main-post-handler req type url-parts-raw)
  (define url-parts (filter non-empty-string? url-parts-raw))
  (define ans
    (match type
      ["fs" (fs-handler req url-parts (get-op-hash))]
      ;; dump handler for testing
      ["dump" (dump-handler req url-parts)]))
  ;;; convert ans to jsexp
  (response/text (jsexpr->bytes ans)))

(define (main-get-handler req type url-parts-raw)
  (define url-parts (filter non-empty-string? url-parts-raw))
  (define ans
    (match type
      ["fs" (get-contents-handler req url-parts)]
      ["config" (get-config-handler req url-parts do-get-config)]
      ["render" (render-handler req url-parts do-render)]
      ["search" (search-handler req url-parts)]
      ["tags" (get-config-handler req url-parts do-get-tags)]
      ["watch" (watch-handler req url-parts do-watch)]
      ;; dump handler for testing
      ["dump" (dump-handler req url-parts)]))
  ;;; convert ans to jsexp
  (response/text (jsexpr->bytes ans)))
