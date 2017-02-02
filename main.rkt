#lang racket

(require web-server/servlet
         web-server/servlet-env)
(require web-server/templates)
(require web-server/dispatchers/dispatch)
(require xml)
(require "config.rkt")
(require "view.rkt")
(require "ctrl.rkt")
(require "util.rkt")


;; Request -> string
;; return request resource string (path relative to the webapp's root)
(define/contract (request->resource req)
  (-> request? resource?)
  (define uri (request-uri req))
  (define resource (map path/param-path (url-path uri)))
    (string-append "/" (string-join resource "/")))


;; Debug use
(define (print-request req)
  (let ((uri (request-uri req)))
    (println (format "query: ~a" (url-query uri)))
    (println (format "path: ~a" (url-path uri)))
    (println (format "bindings: ~a" (request-bindings/raw req)))
    (println (format "post-data: ~a" (request-post-data/raw req)))))

;; For GET request, this function fetch key and value from req
(define/contract (get-query key req)
  (-> symbol? request? (or/c pair? #f))
  (let ((uri (request-uri req)))
    (assoc key (url-query uri))))


;; Handlers begin

(define (dispatch-handler req)
  (define resource (request->resource req))
  ;; construct absoluate path (abs as in unix /) for testing
  (define abs-path (append-path webroot resource))
  (let ((dir-exist (directory-exists? abs-path))
        (file-exist (file-exists? abs-path)))
    (cond [(string-prefix? resource "/.json")
           (API-handler req)]
          [(and (not dir-exist) (not file-exist))
           #|(response/xexpr
            `(html (head (title "404"))
                   (body (p "file " ,resource " not exists."))))|#
           (next-dispatcher)
           ]
          [dir-exist
           ;; this is a directory, pass in requested path
           (list-file-handler req)]
          [file-exist
           ;; this is a file, pass in request
           (process-file-handler req)])))

;;; API request
(struct Autosave (resource text)
        #:transparent
        #:guard (lambda (f t tmp)
                  (unless (string-prefix? f "/")
                    (error 'Autosave (format "corruptted resource name. Supposed to be absolute path inside webroot, but got ~a" f)))
                  (values f t)))

(define (request-api-type req)
  (let ((bindings (request-bindings req)))
    (extract-binding/single 'cmd bindings)))

(define (request->autosave req)
  (let ((bindings (request-bindings req)))
    (let ((resource (extract-binding/single 'resource bindings))
          (text (extract-binding/single 'text bindings)))
      (Autosave resource text))))

;; Handle API calling.
;;
;; autosave:
;; 1. save text into resource (truncate resource before saving).
;; 2. problem could be slow file system operation and saving commands
;;    stacks up.
(define/contract (API-handler req)
  (-> request? response?)

  (define (handle-autosave autosave)
    (define filepath (append-path webroot (Autosave-resource autosave)))
    (cond [(not (file-exists? filepath)) #f]
          [else
           (call-with-output-file* filepath
             (lambda (out)
               (display (Autosave-text autosave) out))
             #:mode 'text
             #:exists 'must-truncate)]))

  (match (request-api-type req)
    ["autosave"
     (let ((autosave (request->autosave req)))
       (let ((saved? (handle-autosave autosave)))
         (response/full
          200 #"Okay"
          (current-seconds)
          TEXT/HTML-MIME-TYPE
          empty
          (list (if saved? #"saved" #"error occurs. Manually save your text")))))]
    [x
     (response/xexpr
      `(html (head) (p ,(format "unknown API: ~a" x))))]))

;;; original request wants to list directory, do it in this handler
(define (list-file-handler req)
  ;; render filepath to breadcrumb
  (define resource (request->resource req))
  (define breadcrumb (xexpr/resource->breadcrumb resource))

  ;; sort files: folders come before files.
  (define all-files (ctrl/all-files resource))
  (define-values (folderobjs fileobjs)
    (partition ctrl/file-is-directory? all-files))
  (let ((folders (xexpr/all-files folderobjs))
        (files   (xexpr/all-files fileobjs)))
    (response/full
     200 #"Okay"
     (current-seconds)
     TEXT/HTML-MIME-TYPE
     empty
     (list (string->bytes/utf-8 (include-template "files.html"))))))

;;; request an existing file
(define ext->mime-type
  #hash((#""     . #"text/html; charset=utf-8")
        (#"html" . #"text/html; charset=utf-8")
        (#"png"  . #"image/png")
        (#"css"  . #"text/css")
        (#"rkt"  . #"text/x-racket; charset=utf-8")))

(define (process-file-handler req)
  (define edit? (get-query (string->symbol query-edit-name) req))
  (define resource (request->resource req))
  (if (ctrl/has-pollen-suffix? resource)
      (pollen-file-handler req resource edit?)
      (static-file-handler resource)))

(define (pollen-file-handler req resource edit?)
  (define breadcrumb (xexpr/resource->breadcrumb resource))
  (define filepath (append-path webroot resource))
  (define data (file->bytes filepath))
  (define (view-file-handler)
    (response
     200 #"Okay"
     (current-seconds)
     TEXT/HTML-MIME-TYPE
     empty
     (lambda (out) (write-bytes data out))))
  (define (edit-file-handler)
    (let ((content data))
      (response/full
       200 #"Okay"
       (current-seconds)
       TEXT/HTML-MIME-TYPE
       empty
       (list (string->bytes/utf-8
              (include-template "editor.html"))))))
  (if edit?
      (edit-file-handler)
      (view-file-handler)))

(define (static-file-handler resource)
  (define file (append-path webroot resource))
  (define ext (filename-extension file))
  (define mime-type
    (hash-ref ext->mime-type ext TEXT/HTML-MIME-TYPE))
  (define data (file->bytes file))
  (response
   200 #"Okay"
   (current-seconds)
   mime-type
   empty
   (lambda (out) (write-bytes data out))))

(serve/servlet dispatch-handler
               #:port 8000
               #:launch-browser? #f
               #:servlet-regexp #rx""
			   #:extra-files-paths (list runtimeroot))

