#lang racket

(require web-server/servlet
         web-server/servlet-env)
(require web-server/templates)
(require web-server/dispatchers/dispatch)
(require web-server/dispatch)

(require setup/getinfo)
(require pollen/file)
(require pollen/render)
(require "config.rkt")
(require "http-util.rkt")
(require "logger.rkt")
(require racket/cmdline)
(require racket/logging)

(require (prefix-in restful: "restful.rkt"))

(provide start-servlet)

(define (static-file-handler req url-parts)
  (log-web-request-debug "accessing ~s" url-parts)
  (let ((nonempty-urls (filter non-empty-string? url-parts)))
    (define filepath
      (apply build-path (cons (current-directory) nonempty-urls)))
    (render-from-source-or-output-path filepath)
    (next-dispatcher)))

(define (editor-handler req url-parts)
  (log-web-request-debug "editor on url-parts")
  (response/text
   (file->string
    (build-path editor-root "index.html"))))


(define-values (server-dispatch url)
  (dispatch-rules
   [("rest" (string-arg) (string-arg) ...) #:method "post"
    restful:main-post-handler]
   [("rest" (string-arg) (string-arg) ...)
    restful:main-get-handler]
   [("editor" (string-arg) ...)
    editor-handler]
   [((string-arg) ...) static-file-handler]))


(define (start-servlet ip port)
  (serve/servlet server-dispatch
                 #:command-line? #t
                 #:banner? #f
                 #:port port
                 #:listen-ip ip
                 #:launch-browser? #f
                 #:servlet-regexp #rx""
                 #:extra-files-paths (list (current-directory) editor-root)
                 #:servlet-current-directory (current-directory)))

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
       ("Make pollen server accessible by only this machine")]
      [("--start-dir")
       ,(lambda (flag start-dir)
          (current-directory start-dir))
       ("Set server's working dir (default is current dir)" "start-dir")]
      [("--log-level")
       ,(lambda (flag level)
          (let [(level-sym (string->symbol level))]
            (unless (member level-sym all-levels)
              (raise-user-error (format "Unknown log-level ~a" level-sym)))
            (log-level level-sym)))
       (,(format "set log level (default ~a). Choices are ~a"
                 (log-level) (string-join (map symbol->string all-levels) ", "))
        "level")]))
   (lambda (f) (void))
   '())

  (printf "Welcome to Pollen Rock ~a (Racket ~a)\n"
          ((get-info/full runtimeroot) 'version)
          (version))
  (printf "Project root is ~a\n" (current-directory))
  (printf "Pollen Editor is running at http://localhost:~a/editor (accessible by ~a)\n"
          (server-port)
          (if (listen-ip)
              "only this machine"
              "all machines on your network"))
  (displayln "Ctrl-C at any time to terminate Pollen Rock.")

  (start-logging-threads)
  (start-servlet (listen-ip) (server-port)))

(module+ main
  (start-server))

(module+ raco
  (start-server))
