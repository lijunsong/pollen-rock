#lang racket

(require web-server/http/request-structs)
(require json)
(require "config.rkt")
(require "util.rkt")
(require "http-util.rkt")
(require "logger.rkt")
(require (prefix-in pollen: pollen/file))
(require (prefix-in pollen: pollen/render))
(require (only-in "fs-watch.rkt" file-watch))
(require sugar)
(require "handlers/fs.rkt")

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
      ["fs" (fs-handler req url-parts (get-op-hash))]))
  ;;; convert ans to jsexp
  (response/text (jsexpr->bytes ans)))




