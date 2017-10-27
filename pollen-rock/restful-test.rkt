#lang racket

(require rackunit)
(require net/url-string)
(require web-server/http/request-structs)
(require web-server/http/response-structs)
(require "restful.rkt")

;;; test /fs/$path

(define/contract (make-fs-request src opname data)
  (-> bytes? bytes? (or/c bytes? false?) request?)
  (make-request
   #"POST" (string->url (format "http://localhost:8000/rest/fs~a" src)) empty
   (delay
     (cons (make-binding:form #"op" opname)
           (if (false? data) empty (list (make-binding:form #"data" data)))))
   #"fake post not used"
   "0.0.0.0"
   8000
   "0.0.0.0"))

(define mv-bad-request (make-fs-request #"/src/path1" #"mv" false))

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
(define mv-request (make-fs-request #"/src/path1" #"mv" #"/dst"))
(check-equal?
 (fs-handler mv-request (list "src" "path1")
             (hash #"mv" (lambda (src dst)
                           (check-equal? src "/src/path1")
                           (check-equal? dst "/dst"))))
 (fs-answer 0))
