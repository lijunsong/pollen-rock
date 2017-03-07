#lang racket

;;; Note: this file works with racket 6.5 and above for use of
;;; functions like path-get-extension
(require "config.rkt")
(require "util.rkt")
(provide (all-defined-out))

;; file represents a file on disk. Since this is for web app, file
;; struct always stores a absolute path which is relative to webroot.
(struct file (name resource directory? ptype)
        #:transparent
        #:guard (lambda (name res dir? ptype tmp)
                  (unless (resource? res)
                    (error 'file (format "resource is not absolute path: ~a" res)))
                  (values name res dir? ptype)))

(define suffix-map #hash((".pp" . pp)
                         (".pm" . pm)
                         (".pmd" . pmd)
                         (".p" . p)))
;;
;; path-string? -> PollenType
;; where PollenType is one of #f, 'pp, 'pm, 'pmd, 'p
(define (data/pollentype fname)
  (let ((ext (path-get-extension fname)))
    (if (eq? ext #f)
        #f
        (hash-ref suffix-map (bytes->string/utf-8 ext) #f))))

;; return file struct for all files 
(define/contract (data/all-files resource)
  (-> resource? (listof file?))
  (define disk-path (append-path webroot resource))
  (unless (directory-exists? disk-path)
    (error 'data/all-files
           (format "resource ~a not found on ~a." resource webroot)))
  (define names (map path->string (directory-list disk-path)))
  (define urls  (map (lambda (n)
                       (path-elements->resource
                        `(,@(resource->path-elements resource) ,n)))
                     names))
  (define files-dir? (map directory-exists?
                          (map (lambda (n) (append-path disk-path n)) names)))
  (define pollen-types (map data/pollentype urls))
  (map file names urls files-dir? pollen-types))

