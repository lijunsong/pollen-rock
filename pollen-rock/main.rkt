#lang racket

(require web-server/servlet
         web-server/servlet-env)
(require web-server/templates)
(require web-server/dispatchers/dispatch)
(require web-server/dispatch)
(require pollen/file)
(require pollen/render)
(require xml)
(require "config.rkt")
(require "view.rkt")
(require "ctrl.rkt")
(require "util.rkt")
(require "api.rkt")
(require "http-util.rkt")
(require racket/cmdline)

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

;; Handler for indexing a local folder or file
;;
;; if the req is not referring to a folder, test if its pollen
;; source. If not, yield to next dispatcher
(define (index-handler req _)
  (define resource (request->resource req))
  (cond [(directory-exists? (append-path webroot resource))
         ;; render directory path to breadcrumb
         (define breadcrumb (xexpr/resource->breadcrumb resource))
         ;; split the list into folders and files.
         (define all-files (ctrl/all-files resource))
         (define-values (folderobjs fileobjs)
           (partition ctrl/file-is-directory? all-files))
         (let ((folders (xexpr/all-files folderobjs))
               (files   (xexpr/all-files fileobjs)))
           (response/text (include-template "files.html")))]
        [else
         (define file-path (path->complete-path
                            (append-path webroot resource)))
         (let ((pollen-source (get-source file-path)))
           (unless (eq? pollen-source #f)
             (render-to-file-if-needed pollen-source))
           (next-dispatcher))]))

(define (edit-handler req trimmed-url)
  (define resource (string-list->resource trimmed-url))
  (define breadcrumb (xexpr/resource->breadcrumb resource))
  (define filepath (append-path webroot resource))
  (define content (file->bytes filepath))
  (response/text (include-template "editor.html")))

(define-values (server-dispatch url)
  (dispatch-rules
   [("edit" (string-arg) ...) edit-handler]
   [("api") #:method "post" api-post-handler]
   [((string-arg) ...) index-handler]))

(define server-port (make-parameter 8000))

;; add runtimeroot to serve pollen-rock's static files when indexing.
;; add webroot to serve users' own static files
(define (start-server)
  (parse-command-line
   "pollen-rock"
   (current-command-line-arguments)
   `((once-each
      [("-p" "--port")
       ,(lambda (flag port)
          (server-port (string->number port)))
       (,(format "server's port (default ~a)" (server-port))
        "port")]))
   (lambda (f) (void))
   '())
  (serve/servlet server-dispatch
                 #:port (server-port)
                 #:listen-ip "0.0.0.0"
                 #:launch-browser? #f
                 #:servlet-regexp #rx""
                 #:extra-files-paths (list webroot runtimeroot)
                 #:servlet-current-directory webroot))

(module+ main
  (start-server))

(module+ raco
  (start-server))
