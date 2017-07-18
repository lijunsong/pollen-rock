#lang racket

(require web-server/servlet
         web-server/servlet-env)
(require web-server/templates)
(require rackunit)
(require xml)
(require "config.rkt")
(require "ctrl.rkt")
(require "util.rkt")

(provide (all-defined-out))

;;; View begin. All functions return xexpr

;;; First binding functions for css library
;; icon
(define/contract (ui-icon name)
  (-> string? xexpr?)
  `(i ((class "material-icons")) ,name))

;; convert filetype to an icon
(define/contract (ui-file-icon is-dir)
  (boolean? . -> . xexpr?)
  (let ((name (if is-dir "folder" "description")))
    (ui-icon name)))

;; button
(define (ui-btn text)
  (let ((txt (if (list? text) text (list text))))
    `(a ((class "btn-flat")) ,@txt)))

(define/contract (ui-collections items)
  (-> (listof xexpr?) (or/c empty? xexpr?))
  (if (empty? items)
      '()
      `(ul ((class "collection"))
           ,@(map (lambda (i) `(li ((class "collection-item")) ,i))
                  items))))

;; return the view of all files
(define (xexpr/all-files files)
  (define names  (map ctrl/file-name files))
  (define resources   (map ctrl/file-resource files))
  (define is-dir (map ctrl/file-is-directory? files))
  (define ptypes  (map ctrl/file-ptype files))

  (define (link name url ptype)
    (let ((modified-url (if (or ptype (string-suffix? name ".rkt"))
                            (url-mark-edit url) url))
          (watch-url (if ptype (url-mark-watch url) #f)))
      `(div (span ((class "edit-url")) (a ((href ,modified-url)) ,name))
            ,@(if watch-url
                 `((span ((class "watch-url")) (a ((class "btn") (href ,watch-url)) "Watch")))
                 `()))))
  (ui-collections (map link names resources ptypes)))

(define/contract (xexpr/resource->breadcrumb resource)
  (-> resource? xexpr?)

  (define urls (resource->breadcrumb-url resource))
  `(div ((class "breadcrumb-list"))
        ,@(map (lambda (name url)
                 `(a ((href ,url) (class "breadcrumb-item")) ,name))
               (cons "Home" (string-split resource "/"))
               (cons "/" urls))))
