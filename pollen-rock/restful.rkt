#lang racket

(require web-server/http/request-structs)
(require json)
(require "http-util.rkt"
         "logger.rkt"
         "handlers/fs-handler.rkt"
         "handlers/get-config-handler.rkt"
         "handlers/render-handler.rkt"
         "handlers/watch-handler.rkt"
         "handlers/dump-handler.rkt")

(provide (all-defined-out))

;; To add more handlers for different route, add restful route in
;; main-handler, and define a sub-handler accepting request and
;; url-parts (path elements)
;;
;; NOTE: sub handlers must return a Jsexpr


;; main handler dispatch request to different handlers.
(define (main-handler req type url-parts)
  (print-request req)
  (define ans
    (match type
      ["fs" (fs-handler req url-parts (get-op-hash))]
      ["config" (get-config-handler req url-parts do-get-config)]
      ["watch" (watch-handler req url-parts do-watch)]
      ["render" (render-handler req url-parts do-render)]
      ;; dump handler for testing
      ["dump" (dump-handler req url-parts)]))
  ;;; convert ans to jsexp
  (response/text (jsexpr->bytes ans)))
