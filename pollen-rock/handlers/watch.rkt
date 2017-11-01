#lang racket

;;;; POST /watch/$path
(define/contract (watch-answer errno mtime)
  (-> integer? integer? jsexpr?)
  (hasheq 'errno errno
          'mtime mtime))

(define/contract (watch-handler req url-parts watching)
  (-> request? (listof string?)
      (-> string? (or/c false? integer?) jsexpr?) jsexpr?)
  (define url-path (path-elements->resource url-parts))
  (define file-path (append-path webroot url-path))
  (define bindings (request-bindings/raw req))
  ;; we may or may not get mtime from request
  (define mtime (extract-bindings #"mtime" bindings))
  (with-handlers
      ([exn:fail:filesystem? (lambda (e) (watch-answer 1 0))])
    (define int-mtime (string->number (bytes->string/utf-8 mtime)))
    (watching file-path int-mtime)))

(define/contract (watching-file file-path last-seen-seconds)
  (-> string? (or/c false? integer?) jsexpr?)
  (define ans (watch-answer 0 0))
  ;; when the file is changed, update this ans, and
  ;; return ans.

  (define (update-mtime _ last-mod)
    (hash-set! ans 'mtime last-mod))
  (when (file-watch file-path update-mtime last-seen-seconds)
    ans))


(module+ test
  (require rackunit)
;;;; test watch-handler

(define watch-request-url-parts
  (list "watch" "this" "file.txt"))

(define watch-request
  (make-test-request "watch" watch-request-url-parts
                     (hash #"mtime" #"100")))

;; test watch-handler returns jsexp when the watching file doesn't exist
(check-equal?
 (watch-answer 1 0)
 (watch-handler watch-request (list "watch" "this" "file.txt")
                (lambda (path mtime)
                  (raise (make-exn:fail:filesystem
                          "exists"
                          (current-continuation-marks)))
                  (watch-answer 0 0))))

;; test watch-handler passes correct path and mtime to watching function
(check-equal?
 (watch-answer 0 1001)
 (watch-handler watch-request (list "watch" "this" "file.txt")
                (lambda (path mtime)
                  (check-true (string-contains? path "/watch/this/file.txt"))
                  (check-equal? mtime 100)
                  (watch-answer 0 1001))))



  )
