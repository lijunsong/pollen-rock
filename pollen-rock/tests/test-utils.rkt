#lang racket

(require rackunit)
(require json)
(require racket/port)
(require net/http-client)
(require net/uri-codec)

(provide (all-defined-out))

(define HOST "127.0.0.1")
;; (define PORT 34732)
(define PORT 8000)

(define/contract (get-conn)
  (-> http-conn?)
  (define conn (http-conn))
  (http-conn-open! conn HOST #:port PORT)
  conn)


;; check the status is not 500
(define-simple-check (check-status-not-500? status)
  (define status-str
    (if (bytes? status) (bytes->string/utf-8 status) status))
  (not (string-contains? status-str "500")))

;; check servlet status contains the given code
(define-binary-check (check-status-contains? status code)
  (define status-str
    (if (bytes? status) (bytes->string/utf-8 status) status))
  (string-contains? status-str (number->string code)))

;; Post to the uri with data in alist. check will be called with the
;; response converted to jsexpr. The status of the response is also
;; checked.
(define/contract (check-post-request hc uri alist check)
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
       ;; no server error should occur
       (check-status-not-500? status)
       (check (string->jsexpr ans))))))


;; In a given procedure, check the response status, headers and body
;; of get method on uri. The status of the reponse is also checked.
(define/contract (check-get-response hc uri check)
  (-> http-conn? string? (-> bytes? (listof bytes?) string? void?) void?)
  (check-not-exn
   (lambda ()
     (let-values
         ([(status headers in)
           (http-conn-sendrecv!
            hc uri #:method #"GET")])
       (check status headers (port->string in))))))

;; Check the response contents in a given procedure
(define/contract (check-get-response-contents hc uri check)
  (-> http-conn? string? (-> string? void?) void?)
  (check-get-response hc uri
                     (lambda (status headers contents)
                       (check contents))))

;; Check if the response status is the given integer
(define-check (check-get-response-status hc uri code)
  (check-get-response hc uri
                     (lambda (status headers contents)
                       (check-status-contains? status code))))

;; send GET request with timeout guard.
;;
;; return false if the request is timed out, or a list of three
;; elements (status headers contents)
(define/contract (get-request/timeout hc uri timeout)
  (-> http-conn? string? (or/c false? (not/c negative?))
      (or/c false? list?))
  ;; create a new thread do sendrecv, and put the result into a
  ;; channel. Sync the channel and timeout evt to get the result.
  (define response-ch (make-channel))
  (define th
    (thread
     (lambda ()
       (define-values
         (status headers in)
         (http-conn-sendrecv! hc uri #:method #"GET"))
       (channel-put response-ch
                    (list status headers (port->bytes in))))))
  (sync/timeout timeout response-ch))
