#lang racket

(require racket/runtime-path)

(provide (all-defined-out))

;;; Command Line options

;; server's port
(define server-port (make-parameter 8000))
(define listen-ip (make-parameter #f))
(define log-level (make-parameter 'info))

;;; Web root for looking for resources
(define-runtime-path runtimeroot ".")
