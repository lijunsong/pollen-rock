#lang racket

;;; API: get filesystem contents (list directory or get file contents)


(require "../http-util.rkt")
(require "../logger.rkt")
(require json)
(require web-server/http/request-structs)

(provide get-contents-handler)


;; return spec for file operation
(define/contract (get-contents-answer errno [mtime 0] [items false] [contents false])
  (->* (integer?) (integer? (or/c list? false) (or/c string? false)) jsexpr?)
  (cond [(and (eq? items false) (eq? contents false) (= errno 0))
         (error 'get-contents-answer
                "items and contents must not be empty at the same time")]
        [(not (= errno 0))
         (hasheq 'errno errno)]

        [(eq? items false)
         (hasheq 'errno errno
                 'mtime mtime
                 'contents contents)]
        [else
         (hasheq 'errno errno
                 'mtime mtime
                 'items items)]))

;; select matched handler from `handler-map` to respond to the `req`.
;; the value of handler-map must be taking one or two positional
;; arguments. If it takes one argument, url-parts will be converted to
;; a path and passed into the function. If two arguments, `req`
;; binding `data` will be passed as bytes to the second argument.
(define (get-contents-handler req url-parts)
  (log-rest-info "get-contents-handler ~a" url-parts)
  ;; I have to inline the filter. See comments in static-file-handler
  (define file-path
    (apply build-path `(,(current-directory)
                        ,@(filter non-empty-string? url-parts))))
  (cond [(directory-exists? file-path)
         (log-rest-debug "list directory ~a" file-path)
         (define items (directory-list file-path #:build? false))
         (define names (map (lambda (n)
                              (if (directory-exists? (build-path file-path n))
                                  (string-append (path->string n) "/")
                                  (path->string n)))
                            items))
         (define mtime (file-or-directory-modify-seconds file-path))
         (get-contents-answer 0 mtime names)]
        [(file-exists? file-path)
         (log-rest-debug "get contents of file ~a" file-path)
         (define contents (file->string file-path))
         (define mtime (file-or-directory-modify-seconds file-path))
         (get-contents-answer 0 mtime false contents)]
        [else
         (get-contents-answer 1)]))
