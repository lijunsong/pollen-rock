#lang racket

(require "config.rkt")

(provide (all-defined-out))

(define-logger web-request)
(define-logger api)

(define all-levels '(none fatal error warning info debug))

;; create receiver only after (log-level) has been set correctly.
(define (start-logging-threads)
  (println (format "start-logging-threads at ~a" (log-level)))
  (define web-request-rc (make-log-receiver web-request-logger (log-level)))
  (define api-rc (make-log-receiver api-logger (log-level)))

  (void
   (thread
    (lambda ()
      (let loop ()
        (define v (sync web-request-rc api-rc))
        (define level (vector-ref v 0))
        (printf "[~a] ~a\n" level (vector-ref v 1))
        (loop))))))
