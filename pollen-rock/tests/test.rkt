#lang racket

;; This test opens a process to launch the ReSTful Server, and sends
;; GET and POST HTTP requests to the server.
;;
;; This directory contains pollen files used to exercise API.

(require rackunit)
(require json)
(require racket/port)
(require racket/file)
(require net/http-client)
(require net/uri-codec)

(require "../config.rkt")
(require "../main.rkt")
(require "../handlers/fs-handler.rkt")
(require "test-utils.rkt")

(provide (all-defined-out))

(define/contract (launch-pollen-rock)
  (-> thread?)
  (thread
   (lambda _ (start-servlet HOST PORT))))

(define CODE "#lang racket
(provide (all-defined-out))
(define x 1)
(define (tag0 x . y)
  `(mytag ,x ,@y))
")

;; similar to shell "echo v > path"
(define (create-file path v)
  (display-to-file v path
                   #:mode 'text
                   #:exists 'replace))

;;;;;;;;;;;;;;;; BEGIN TESTING POST ;;;;;;;;;;;;;;;;;;
(define root-path "project-root")
(define old-current-directory (current-directory))

;; remove the test project root and create a new one
(delete-directory/files root-path #:must-exist? false)
(make-directory root-path)

;; NOTE: really important! Switch into root-path to test
(current-directory root-path)

;; start pollen-rock server
(define server (launch-pollen-rock))

;; wait for server up
(sleep 3)

;; connect to the server
(define conn-post (get-conn))

;; actual: jsexpr returned as response from the server
;; errno: the expected errno in actual
(define-check (check-errno actual errno)
  (when (not (jsexpr? actual))
    (fail-check (format "not a json: ~s" actual)))
  (let [(actual-no (hash-ref actual 'errno false))]
    (when (eq? actual-no false)
      (fail-check (format "json doesn't has 'errno field: ~s" actual )))
    (when (not (equal? actual-no errno))
      (fail-check (format "expected errno is ~a, but is ~a"
                          errno actual-no)))))

(test-case
 "mkdir when dest doesn't exist"
 (check-post-request conn-post "rest/fs/folder1"
             `((op . "mkdir"))
             (lambda (res)
               (check-errno res 0)))
 (check-true (directory-exists? "folder1")))

(test-case
 "mkdir when dest exists"
 (check-post-request conn-post "rest/fs/folder1"
             `((op . "mkdir"))
             (lambda (res)
               (check-errno res 1))))

(test-case
 "write when src doesn't exist"
 (check-post-request conn-post "/rest/fs/file1.html.pm"
             `((op . "write")
               (data . "test string"))
             (lambda (res)
               (check-errno res 0)))
 (check-true (file-exists? "file1.html.pm"))
 (check-equal? (file->string "file1.html.pm")
               "test string"))

;; test write: src exists. overwrite
(test-case
 "write when src exists (overwrite)"
 (check-post-request conn-post "/rest/fs/file1.html.pm"
             `((op . "write")
               (data . ,CODE))
             (lambda (res)
               (check-errno res 0)))
 (check-true (file-exists? "file1.html.pm"))
 (check-equal? (file->string "file1.html.pm")
               CODE))

;; test write: src is a directory.
(test-case
 "write to a directory"
 (check-post-request conn-post "/rest/fs/folder1"
             `((op . "write")
               (data . "second pass"))
             (lambda (res)
               (check-errno res 1)))
 (check-true (file-exists? "file1.html.pm"))
 (check-equal? (file->string "file1.html.pm")
               CODE))

;; test mv: src folder exists; dst does not. equivalent to rename
(test-case
 "mv folder when src exists and dest doesn't"
 (check-post-request conn-post "/rest/fs/folder1"
             `((op . "mv")
               (data . "folder2"))
             (lambda (res)
               (check-errno res 0)))
 (check-pred (negate directory-exists?) "folder1")
 (check-pred directory-exists? "folder2"))

;; test mv: src file exists; dst does not. equivalent to rename
(test-case
 "mv file when src eixsts and dest doesn't"
 (check-post-request conn-post "/rest/fs/file1.html.pm"
             `((op . "mv")
               (data . "file2.html.pm"))
             (lambda (res)
               (check-errno res 0)))
 (check-pred (negate file-exists?) "file1.html.pm")
 (check-pred file-exists? "file2.html.pm"))


;; test mv: src doesn't exist
(test-case
 "mv when src doesn't exist"
 (check-post-request conn-post "/rest/fs/unknown-folder1"
             `((op . "mv")
               (data . "unknown-folder2"))
             (lambda (res)
               (check-errno res 1))))

(test-case
 "mv when src and dst both exist"
 (check-post-request conn-post "/rest/fs/folder1"
             `((op . "mkdir"))
             (lambda (res)
               (check-errno res 0)))

 (check-post-request conn-post "/rest/fs/folder1"
             `((op . "mv")
               (data . "folder2"))
             (lambda (res)
               (check-errno res 1))))

(test-case
 "rm when src doesn't exist"
 (check-post-request conn-post "/rest/fs/folder3"
             `((op . "rm"))
             (lambda (res)
               (check-errno res 1))))

;; test rm: src exists
(test-case
 "rm when src exists"
 (check-post-request conn-post "/rest/fs/folder3"
             `((op . "mkdir"))
             (lambda (res)
               (check-errno res 0)))
 (check-post-request conn-post "/rest/fs/folder3/a-file.txt"
             `((op . "write")
               (data . "some data"))
             (lambda (res)
               (check-errno res 0)))

 (check-post-request conn-post "/rest/fs/folder3"
             `((op . "rm"))
             (lambda (res)
               (check-errno res 0)))
 (check-pred (negate directory-exists?) "folder3"))

;; TODO: rm "rest/fs/." / rm "/rest/fs//"

;; test ls: src exists
(test-case
 "ls when src exists"
 (define n-files 10)
 (check-post-request conn-post "/rest/fs/folder4"
             `((op . "mkdir"))
             (lambda (res) (check-errno res 0)))
 (for [(i (range n-files))]
   (check-post-request conn-post (format "/rest/fs/folder4/file~a.txt" i)
               `((op . "write")
                 (data . "data"))
               (lambda (res) (check-errno res 0))))
 (check-post-request
  conn-post "/rest/fs/folder4"
  `((op . "ls"))
  (lambda (res)
    (define files (hash-ref res 'message false))
    (if (not (list? files))
        (fail (format "No list found. Reponse is ~s" res))
        (check-equal?
         (sort files string<?)
         (map (lambda (i)
                (format "file~a.txt" i))
              (range n-files)))))))



;;;;;;;;;;;;;;;; BEGIN TESTING GET ;;;;;;;;;;;;;;;;;;

#|
(test-case
 "check server doesn't crash for GET /"
 (define conn-get (get-conn))
 (check-get-status-contains? conn-get "/" 200))

(test-case
 "check server doesn't crash for GET a directory"
 (define conn-get (get-conn))
 (check-get-status-contains? conn-get "/" 200))
|#

(test-case
 "check server doesn't crash for GET a pm file"

 (define conn (get-conn))
 (define PM-CONTENT "#lang pollen\nThis is a file for testing!")

 (check-post-request
  conn "/rest/fs/file1.html.pm"
  `((op . "write")
    (data . ,PM-CONTENT))
  (lambda (res) (check-errno res 0)))

;; TODO: also check the returned contents
 (check-get-response-status conn "/file1.html.pm" 200))

(test-case
 "get config of a pm file"
 (define conn (get-conn))
 (check-get-response
  conn "/rest/config/file1.html.pm"
  (lambda (status headers contents)
    (check-status-not-500? status)
    (check-errno (bytes->jsexpr contents) 0))))


(test-case
 "watch handler shouldn't return if there is no change to the file"
 (define conn (get-conn))
 (create-file "file1.html.pm" CODE)
 (define ans (get-request/timeout conn "/rest/watch/file1.html.pm" 3))
 ;; if request is timed out, the result is false.
 (check-false ans))

(test-case
 "watch handler should return only when file has changed"
 (define conn (get-conn))
 (create-file "file1.html.pm" CODE)

 ;; touch thread sleeps 2 seconds and then touchs a file. The main
 ;; thread watches the file and will be timed out for 4 seconds. The
 ;; get-request/timeout should return http response instead false (the
 ;; timeout value)
 (define touch-th (thread (lambda ()
                            (sleep 2)
                            (create-file "file1.html.pm" CODE))))
 (define ans (get-request/timeout conn "/rest/watch/file1.html.pm" 4))
 (check-not-false ans))



;; Clean the test folder
; (parameterize [(current-directory old-current-directory)]
;  (delete-directory/files root-path #:must-exist? false))
