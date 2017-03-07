#lang racket

(require racket/runtime-path)
(require "util.rkt")

(provide (all-defined-out))

;;; Web root for looking for resources
(define webroot (current-directory))
(define-runtime-path runtimeroot ".")

;;; names for dispatching queries

;; add query on url to signify edit action
(define query-edit-name "edit")
(define/contract (url-mark-edit url)
  (-> resource? resource?)
  (path-elements->resource
   (cons query-edit-name (resource->path-elements url))))

;; TODO: this is supposed to return a resource!
(define query-watch-name "watchfile")
(define/contract (url-mark-watch url)
  (-> resource? resource?)
  (path-elements->resource
   (cons query-watch-name (resource->path-elements url))))

(module+ test
  (require rackunit)
  (check-equal? (url-mark-edit "/page1/index.html")
                (path-elements->resource
                 (list query-edit-name "page1" "index.html")))
  (check-equal? (url-mark-watch "/dir1/dir2/index.html")
                (path-elements->resource
                 (list query-watch-name "dir1" "dir2" "index.html"))))
