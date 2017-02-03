#lang racket

(require web-server/servlet
         web-server/servlet-env)
(require web-server/templates)
(require web-server/dispatchers/dispatch)
(require web-server/dispatch)
(require xml)
(require "config.rkt")
(require "view.rkt")
(require "ctrl.rkt")
(require "util.rkt")
(require "api.rkt")

;; assemble url component into a resource
(define/contract (string-list->resource lst)
  (-> (listof string?) resource?)
  (string-append "/" (string-join lst "/")))

;; extract resource from a request
(define/contract (request->resource req)
  (-> request? resource?)
  (define uri (request-uri req))
  (define string-list (map path/param-path (url-path uri)))
  (string-list->resource string-list))

;;; Debug use
(define (print-request req)
  (let ((uri (request-uri req)))
    (println (format "query: ~a" (url-query uri)))
    (println (format "path: ~a" (url-path uri)))
    (println (format "bindings: ~a" (request-bindings/raw req)))
    (println (format "post-data: ~a" (request-post-data/raw req)))))

(define (test-handler req trimmed-url)
  (print-request req)
  (println (format "url: ~s, resource: ~s" trimmed-url (request->resource req)))
  (response/xexpr `(html (p "test!"))))

;; Handler for indexing a local folder
;; if the req is not referring to a folder, yield to next dispatcher.
(define (index-handler req _)
  (define resource (request->resource req))
  (cond ((not (directory-exists? (append-path webroot resource)))
         (next-dispatcher))
        (else
         ;; render filepath to breadcrumb
         (define breadcrumb (xexpr/resource->breadcrumb resource))
         ;; split the list into folders and files.
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
            (list (string->bytes/utf-8 (include-template "files.html"))))))))

(define (edit-handler req trimmed-url)
  (define resource (string-list->resource trimmed-url))
  (define breadcrumb (xexpr/resource->breadcrumb resource))
  (define filepath (append-path webroot resource))
  (define content (file->bytes filepath))
  (response/full
   200 #"Okay"
   (current-seconds)
   TEXT/HTML-MIME-TYPE
   empty
   (list (string->bytes/utf-8
          (include-template "editor.html")))))

(define-values (server-dispatch url)
  (dispatch-rules
   [("edit" (string-arg) ...) edit-handler]
   [("api") #:method "post" api-post-handler]
   [("api") #:method "get" api-get-handler]
   [((string-arg) ...) index-handler]))

;; add runtimeroot to serve pollen-rock's static files when indexing.
;; add webroot to serve users' own static files
(serve/servlet server-dispatch
               #:port 8000
               #:listen-ip "0.0.0.0"
               #:launch-browser? #f
               #:servlet-regexp #rx""
               #:extra-files-paths (list webroot runtimeroot))
