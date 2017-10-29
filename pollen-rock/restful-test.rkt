#lang racket

(require rackunit)
(require net/url-string)
(require web-server/http/request-structs)
(require web-server/http/response-structs)
(require "restful.rkt")
(require "config.rkt")
(require "util.rkt")


(define/contract (make-test-request kind url-parts binding-hash)
  (-> string? (listof string?) (hash/c bytes? bytes?) request?)
  (let [(bindings (hash-map binding-hash
                            (lambda (k v)
                              (make-binding:form k v))))]
    (make-request
     #"POST" (string->url
              (format "http://localhost:8000/rest/~a~a"
                      kind
                      (path-elements->resource url-parts)))
     empty
     (delay bindings)
   #"fake post not used"
   "0.0.0.0"
   8000
   "0.0.0.0")))


;;; test /fs/$path
(define mv-bad-request
  (make-test-request "fs" (list "src" "path1")
                     (hash #"op" #"mv")))

;; helper check functions
(define (check-fs-answer-errno-equal? a b)
  (check-equal? (hash-ref a 'errno) (hash-ref b 'errno)))


;;;; test handle-fileystem-op

;; test it will return fs-answer when bindings are missing
(check-fs-answer-errno-equal?
 (fs-answer 1 "")
 (handle-filesystem-op
  "/hello/go"
  (hash #"op" #"mv")
  (hash #"mv" (lambda (src dst)
                (void)))))

(check-fs-answer-errno-equal?
 (fs-answer 1 "")
 (handle-filesystem-op
  "/hello/go"
  (hash #"unknown" #"unknown")
  (hash #"mv" (lambda (src dst)
                (void)))))

;; test it will pass the correct parameter inside
(check-fs-answer-errno-equal?
 (fs-answer 0)
 (handle-filesystem-op
  "/src"
  (hash #"op" #"myop" #"data" #"second-arg")
  (hash #"myop" (lambda (src)
                  (check-equal? src "/src")))))

(check-fs-answer-errno-equal?
 (fs-answer 0)
 (handle-filesystem-op
  "/src"
  (hash #"op" #"myop" #"data" #"second-arg")
  (hash #"myop" (lambda (src dst)
                  (check-equal? src "/src")
                  (check-equal? dst "second-arg")))))

;; test whether handle-filesystem-op would return what the actual op
;; returns
(check-equal?
 (fs-answer 0 (list "1" "2"))
 (handle-filesystem-op
  "/src"
  (hash #"op" #"ls")
  (hash #"ls" (lambda (src)
                (list "1" "2")))))

;; test whether handle-filesystem-op would return fs-answer when
;; the actual op throws exceptions
(check-equal?
 (fs-answer 1 "s")
 (handle-filesystem-op
  "/src"
  (hash #"op" #"ls")
  (hash #"ls" (lambda (src)
                (raise (make-exn:fail:filesystem
                        "s"
                        (current-continuation-marks)))))))

;; test handle-filesystem-op always returns error 1
(check-equal?
 (fs-answer 1 "s")
 (handle-filesystem-op
  "/src" (hash #"op" #"ls")
  (hash #"ls" (lambda (src)
                (raise (make-exn:fail:filesystem:errno
                        "s"
                        (current-continuation-marks)
                        (cons 129 'windows)))))))

;;;; test fs-handler
;; exercise the full path from accepting request to handle request
(define mv-request-url-parts (list "src" "path1"))
(define mv-request
  (make-test-request "fs" mv-request-url-parts
                     (hash #"op" #"mv"
                           #"data" #"/dst")))

(check-equal?
 (fs-handler mv-request mv-request-url-parts
             (hash #"mv" (lambda (src dst)
                           (check-equal? src "/src/path1")
                           (check-equal? dst "/dst"))))
 (fs-answer 0))


;;;; test render-handler
(define render-url-parts (list "blog" "1.html.pm"))
(define render-request
  (make-test-request "render" render-url-parts
                     (hash)))

;; test render-handler passes the correct source-path
(check-equal?
 (render-answer 0 "/blog/1.html")
 (render-handler
  render-request
  render-url-parts
  (lambda (source-path)
    (check-equal? source-path (append-path webroot "/blog/1.html.pm"))
    true)))

;; test render-handler returns jsexp when renderer throws exceptions
(check-equal?
 (render-answer 1 false)
 (render-handler
  render-request
  render-url-parts
  (lambda (source-path)
    (raise (make-exn:fail:filesystem:exists "s" (current-continuation-marks))))))

;;;; test watch-handler

(define/contract (make-watch-request src)
  (-> bytes? request?)
  (make-request
   #"POST" (string->url (format "http://localhost:8000/rest/watch~a" src)) empty
   (delay (list (make-binding:form #"mtime" #"100")))
   #"fake post not used"
   "0.0.0.0"
   8000
   "0.0.0.0"))


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

;;;;
