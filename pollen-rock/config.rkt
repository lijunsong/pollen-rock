#lang racket

(require racket/runtime-path)

(provide (all-defined-out))

;;; Command Line options

;; server's port
(define server-port (make-parameter 8000))
(define listen-ip (make-parameter #f))
(define log-level (make-parameter 'info))

;;; Runtime root for the whole project
(define-runtime-path runtimeroot "..")
(define editor-root (build-path runtimeroot "editor" "public"))
