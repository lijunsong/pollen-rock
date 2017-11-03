#lang racket

;; This test opens a process to launch the ReSTful Server, and sends
;; GET and POST HTTP requests to the server.
;;
;; This directory contains pollen files used to exercise API.

(require rackunit)
(require json)
(require racket/port)
(require net/http-client)
(require net/uri-codec)

(require "../config.rkt")
(require "../main.rkt")
(require "../handlers/fs-handler.rkt")

(provide (all-defined-out))

(define HOST "127.0.0.1")
                                        ;(define PORT 34732)
(define PORT 8000)

(define/contract (launch-pollen-rock)
  (-> thread?)
  (thread
   (lambda _ (start-servlet HOST PORT))))

(define/contract (post uri alist)
  (-> string? (listof (cons/c symbol? string?))
      (values bytes? (listof bytes?) input-port?))
  (http-sendrecv
   HOST uri
   #:port PORT
   #:method #"POST"
   #:headers (list "Content-Type: application/x-www-form-urlencoded")
   #:data (alist->form-urlencoded alist)))


;; post to the uri with data in alist. Call check
;; with response text field converted to jsexpr.
(define/contract (post-check hc uri alist check)
  (-> http-conn? string? (listof (cons/c symbol? string?))
      (-> jsexpr? void?) void?)
  (check-not-exn
   (lambda ()
     (let-values
         ([(status headers in)
           (http-conn-sendrecv!
            hc uri #:method #"POST"
            #:headers (list "Content-Type: application/x-www-form-urlencoded")
            #:data (alist->form-urlencoded alist))])
       (define ans (port->string in))
       (check (string->jsexpr ans))))))

(define CODE "#lang racket
(provide (all-defined-out))
(define x 1)
(define (tag0 x . y)
  `(mytag ,x ,@y))
")

;;;;;;;;;;;;;;;; BEGIN TESTING ;;;;;;;;;;;;;;;;;;
(define root-path "test-root")
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
(define conn (http-conn))
(http-conn-open! conn HOST #:port PORT)


(define (check-fs-errno response errno)
  (check-equal? (hash-ref response 'errno) errno))

;; test mkdir: dst doesn't exist
(post-check conn "rest/fs/folder1"
            `((op . "mkdir"))
            (lambda (response)
              (check-fs-errno response 0)))
(check-true (directory-exists? "folder1"))

;; test mkdir: dst exists
(post-check conn "rest/fs/folder1"
            `((op . "mkdir"))
            (lambda (response)
              (check-fs-errno response 1)))

;; test write: src not exists
(post-check conn "/rest/fs/file1.html.pm"
            `((op . "write")
              (data . "test string"))
            (lambda (response)
              (check-fs-errno response 0)))
(check-true (file-exists? "file1.html.pm"))
(when (file-exists? "file1.html.pm")
  (check-equal? (file->string "file1.html.pm")
                "test string"))

;; test write: src exists. overwrite
(post-check conn "/rest/fs/file1.html.pm"
            `((op . "write")
              (data . "second pass"))
            (lambda (response)
              (check-fs-errno response 0)))
(check-true (file-exists? "file1.html.pm"))
(when (file-exists? "file1.html.pm")
  (check-equal? (file->string "file1.html.pm")
                "second pass"))

;; test write: src is a directory.
(post-check conn "/rest/fs/folder1"
            `((op . "write")
              (data . "second pass"))
            (lambda (response)
              (check-fs-errno response 1)))
(check-true (file-exists? "file1.html.pm"))
(when (file-exists? "file1.html.pm")
  (check-equal? (file->string "file1.html.pm")
                "second pass"))

;; test mv: src folder exists; dst not. equivalent to rename
(post-check conn "rest/fs/folder1"
            `((op . "mv")
              (data . "folder2"))
            (lambda (response)
              (check-fs-errno response 0)))
(check-true (not (directory-exists? "folder1")))
(check-true (directory-exists? "folder2"))

;; test mv: src file exists; dst not. equivalent to rename
(post-check conn "rest/fs/file1.html.pm"
            `((op . "mv")
              (data . "file2.html.pm"))
            (lambda (response)
              (check-fs-errno response 0)))
(check-true (not (file-exists? "file1.html.pm")))
(check-true (file-exists? "file2.html.pm"))


;; test mv: src doesn't exists
(post-check conn "rest/fs/unknown-folder1"
            `((op . "mv")
              (data . "unknown-folder2"))
            (lambda (response)
              (check-fs-errno response 1)))

;; test mv: src exists; dst exists
(post-check conn "rest/fs/folder1"
            `((op . "mkdir"))
            (lambda (response)
              (check-fs-errno response 0)))

(post-check conn "rest/fs/folder1"
            `((op . "mv")
              (data . "folder2"))
            (lambda (response)
              (check-fs-errno response 1)))

;; test rm remove the folder


;; Clean the test folder
; (parameterize [(current-directory old-current-directory)]
;  (delete-directory/files root-path #:must-exist? false))
