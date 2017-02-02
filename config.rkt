#lang racket

(require racket/runtime-path)

(provide (all-defined-out))

;;; Web root for looking for resources
(define webroot (current-directory))
(define-runtime-path runtimeroot ".")

;;; names for signifying editing page

;; add query on url to signify edit action
(define query-edit-name "edit")
(define/contract (url-mark-edit url)
  (-> string? string?)
  (string-append url "?edit"))
