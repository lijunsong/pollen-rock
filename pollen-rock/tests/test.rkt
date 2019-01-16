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
(require racket/runtime-path)

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

;; similar to shell "mkdir -p path"
(define (create-directory path)
  (unless (directory-exists? path)
    (make-directory path)))


;; given a list of tags extract the tag matched with the name . See a
;; hash specified in tags-answer inside tags-handler.rkt.
(define/contract (get-tag-by-name tags name)
  (-> (listof (hash/c symbol? jsexpr?)) string? (or/c false hash?))
  (cond [(empty? tags) false]
        [else
         (define tag-val (first tags))
         (define tag-name (hash-ref tag-val 'name false))
         (cond [(false? tag-name)
                ;; tag must have name property inside.
                (error 'get-tag-by-name (format "malformed tags: ~a" tags))]
               [(equal? tag-name name) tag-val]
               [else
                (get-tag-by-name (rest tags) name)])]))


;;;;;;;;;;;;;;;; BEGIN TESTING POST ;;;;;;;;;;;;;;;;;;
(define-runtime-path test-dir ".")
(define root-path (build-path test-dir "project-root"))
(printf "use ~a as test root path" root-path)

;; remove the test project root and create a new one
(displayln root-path)

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

;;;;;;;;;;;;;;;; BEGIN TESTING GET ;;;;;;;;;;;;;;;;;;

#|

TODO: implement front end and see what the result is supposed to be.

(test-case
 "check server doesn't crash for GET /"
 (define conn-get (get-conn))
 (check-get-status-contains? conn-get "/" 200))

(test-case
 "check server doesn't crash for GET a directory"
 (define conn-get (get-conn))
 (check-get-status-contains? conn-get "/" 200))
|#

;; test ls: src exists
(test-case
 "ls when src exists"
 (define conn (get-conn))
 (define n-files 10)

 (create-directory "folder4")
 (create-directory "folder4/folderx")
 (create-file (format "folder4/file1.txt") "data")
 (create-file (format "folder4/file2.txt") "data")

 ;; ls list the directory. Directories will has suffix / in directory
 ;; name. File will have file name as is.
 (check-get-response conn "/rest/fs/folder4"
  (lambda (status headers contents)
    (define res (bytes->jsexpr contents))
    (define files (hash-ref res 'items false))
    (if (not (list? files))
        (fail (format "No list found. Reponse is ~s" res))
        (check-equal?
         (sort files string<?)
         (list "file1.txt" "file2.txt" "folderx/"))))))

(test-case
 "ls when src doesn't exist"
 (define conn (get-conn))

 (check-get-response
  conn "/rest/fs/folder10000"
  (lambda (status headers contents)
    (check-errno (bytes->jsexpr contents) 1))))

(test-case
 "ls / (/ seems a exception for url-parts passed to handlers)"
 (define conn (get-conn))

 (check-get-response
  conn "/rest/fs/"
  (lambda (status headers contents)
    (check-errno (bytes->jsexpr contents) 0))))

(test-case
 "Get file contents when src exist"
 (define conn (get-conn))

 (create-file "file101" "mydata")

 (check-get-response
  conn "/rest/fs/file101"
  (lambda (status headers contents)
    (define res (bytes->jsexpr contents))
    (check-equal? (hash-ref res 'contents false)
                  "mydata"))))


(test-case
 "check server doesn't crash for GET a pm file"

 (define conn (get-conn))
 (define PM-CONTENT "#lang pollen\nThis is a file for testing!")

 (create-file "file1.html.pm" PM-CONTENT)

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
 "get config when pollen.rkt has errors"
 (define conn (get-conn))
 (create-file "pollen.rkt" (format "~a\n(define (foo" CODE))
 (create-file "file2.html.pm" "#lang pollen\nhello")
 (check-get-response
  conn "/rest/config/file2.html.pm"
  (lambda (status headers contents)
    (check-status-not-500? status)
    ;; TODO: see TODO in tags-handler, check errno 1
    (check-errno (bytes->jsexpr contents) 0))))


(test-case
 "get tags returns correct tag"
 (define conn (get-conn))
 (create-file "pollen.rkt" CODE)
 (create-file "file2.html.pm" "#lang pollen\nhello")
 (check-get-response
  conn "/rest/tags/file2.html.pm"
  (lambda (status headers contents)
    (check-status-not-500? status)
    (define tags-ans (bytes->jsexpr contents))
    (check-errno tags-ans 0)
    (define tags (hash-ref tags-ans 'tags false))
    (check-not-false tags)
    (let [(tag (get-tag-by-name tags "tag0"))]
      (check-equal? (hash-ref tag 'kind false) "procedure")
      (check-equal? (hash-ref tag 'arity false) 1)
      (check-equal? (hash-ref tag 'arity-at-least false) true))
    (let [(tag (get-tag-by-name tags "x"))]
      (check-equal? (hash-ref tag 'kind false) "variable")
      (check-equal? (hash-ref tag 'value false) 1)
      (check-equal? (hash-ref tag 'type false) "number")))))


(test-case
 "/search when the source exists and the query is querying source"
 (define conn (get-conn))
 (create-directory "file5")
 (create-file "file5/search1.html.pm" "empty")
 (check-get-response
  conn "/rest/search/file5/search1.html.pm"
  (lambda (stsatus headers contents)
    (define res (bytes->jsexpr contents))
    (check-equal? (hash-ref res 'errno null) 0)
    (check-equal? (hash-ref res 'source-exists null) true)
    (check-equal? (hash-ref res 'source null) "file5/search1.html.pm")
    (check-equal? (hash-ref res 'output null) "file5/search1.html"))))

(test-case
 "/search when the source doesn't exist and the query is querying source"
 (define conn (get-conn))
 (check-get-response
  conn "/rest/search/file5/search-x.html.pm"
  (lambda (stsatus headers contents)
    (define res (bytes->jsexpr contents))
    (check-equal? (hash-ref res 'errno null) 0)
    (check-equal? (hash-ref res 'source-exists null) false)
    (check-equal? (hash-ref res 'source null) "file5/search-x.html.pm")
    (check-equal? (hash-ref res 'output null) "file5/search-x.html"))))

(test-case
 "/search when the source exists and the query is querying output"
 (define conn (get-conn))
 (create-directory "file5")
 (create-file "file5/search2.html.pm" "empty")
 (check-get-response
  conn "/rest/search/file5/search2.html"
  (lambda (stsatus headers contents)
    (define res (bytes->jsexpr contents))
    (check-equal? (hash-ref res 'errno null) 0)
    (check-equal? (hash-ref res 'source-exists null) true)
    (check-equal? (hash-ref res 'source null) "file5/search2.html.pm")
    (check-equal? (hash-ref res 'output null) "file5/search2.html"))))

(test-case
 "/search when the source doesn't exist and the query is querying output"
 (define conn (get-conn))
 (check-get-response
  conn "/rest/search/file5/search-y.html"
  (lambda (stsatus headers contents)
    (define res (bytes->jsexpr contents))
    (check-not-equal? (hash-ref res 'errno null) 0))))

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
(delete-directory/files root-path #:must-exist? false)
