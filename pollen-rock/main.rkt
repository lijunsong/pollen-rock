#lang racket

(require web-server/servlet
         web-server/servlet-env)
(require web-server/templates)
(require web-server/dispatchers/dispatch)
(require web-server/dispatch)
(require setup/getinfo)
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
           (response/text (include-template "templates/files.html")))]
        [else
         (define file-path (path->complete-path
                            (append-path webroot resource)))
         (let ((pollen-source (get-source file-path)))
           (unless (eq? pollen-source #f)
             (render-to-file-if-needed pollen-source))
           (next-dispatcher))]))

(define (edit-handler req trimmed-url)
  (define resource (path-elements->resource trimmed-url))
  (define breadcrumb (xexpr/resource->breadcrumb resource))
  (define filepath (append-path webroot resource))
  (define content (file->bytes filepath))
  (response/text (include-template "templates/editor.html")))

(define (watchfile-handler req url)
  (define resource (path-elements->resource url))
  (response/text (include-template "templates/watchfile.html")))

(define-values (server-dispatch url)
  (dispatch-rules
   [("edit" (string-arg) ...) edit-handler]
   [("api") #:method "post" api-post-handler]
   [("watchfile" (string-arg) ...) watchfile-handler]
   [((string-arg) ...) index-handler]))

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
       (,(format "Set server's port (default ~a)" (server-port))
        "port")]
      [("--local")
       ,(lambda (flag)
          (listen-ip "127.0.0.1"))
       ("Make pollen server accessible by only this machine")]))
   (lambda (f) (void))
   '())

  (displayln (format "Welcome to Pollen Rock ~a (Racket ~a)"
                     ((get-info/full (build-path runtimeroot 'up)) 'version)
                     (version)))
  (displayln (format "Project root is ~a" webroot))
  (displayln (format "Pollen Editor is running at http://localhost:~a (accessible by ~a)"
                     (server-port)
                     (if (listen-ip) "only this machine" "all machines on your network")))
  (displayln "Ctrl-C at any time to terminate Pollen Rock.")

  (serve/servlet server-dispatch
                 #:command-line? #t
                 #:banner? #f
                 #:port (server-port)
                 #:listen-ip (listen-ip)
                 #:launch-browser? #f
                 #:servlet-regexp #rx""
                 #:extra-files-paths (list webroot runtimeroot)
                 #:servlet-current-directory webroot))

(module+ main
  (start-server))

(module+ raco
  (start-server))
