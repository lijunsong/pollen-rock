#lang racket

(require web-server/http/request-structs)
(require json)
(require "config.rkt")
(require "util.rkt")
(require "http-util.rkt")
(require "logger.rkt")

(provide (prefix-out restful- (all-defined-out)))

;; To add more handlers for different route, add restful route in
;; main-handler, and define a sub-handler accepting request and
;; matched-url (path elements)
;;
;; NOTE: sub handlers must return a Jsexpr


;; main handler dispatch request to different handlers.
(define (main-handler req type matched-url)
  (print-request req)
  (define ans
    (match type
      ["fs" (fs-handler req matched-url)]))
  ;;; convert ans to jsexp
  (response/text (jsexpr->bytes ans)))


(define/contract (extract-bindings key bindings)
  (-> bytes? (listof binding?) (or/c false? bytes?))
  (let [(form (bindings-assq key bindings))]
    (if form
        (binding:form-value form)
        false)))


;;;; POST /fs/$path
;; file operations
(define (fs-handler req matched-url)
  (struct fsop (op need-data?) #:transparent)
  ;; fs-ops - a map mapping from op names to FsOps
  (define fs-ops
    (hash #"mv" (fsop mv-op true)
          #"mkdir" (fsop mkdir-op false)
          #"rm" (fsop rm-op false)
          #"cat" (fsop cat-op true)
          #"ls" (fsop ls-op false)))
  ;; prepare op arguments
  (define src (path-elements->resource matched-url))
  (define bindings (request-bindings/raw req))
  (define op-name (extract-bindings #"op" bindings))
  (define op (hash-ref fs-ops op-name false))
  ;; run ops
  (cond [(not op)
         (fs-answer 1 "op field not found")]
        [(fsop-need-data? op)
         (let [(data (extract-bindings #"data" bindings))]
           ((op-sanity-checker (fsop-op op)) src data))]
        [else
         ((op-sanity-checker (fsop-op op)) src)]))


;; Return a function that will convert and check the arguments passed
;; to the given func before calling it.
(define (op-sanity-checker func)
  (define (arg->string arg)
    (if (bytes? arg)
        (bytes->string/utf-8 arg)
        arg))
  (define (fs-op-with-handlers op)
    (lambda args
      (with-handlers
          ([exn:fail:filesystem? (lambda (e) (fs-answer 2 "fs error"))])
        (apply op args)
        (fs-answer 0))))
  (lambda args
    (define string-args (map arg->string args))
    (if (andmap non-empty-string? string-args)
        (apply (fs-op-with-handlers func) string-args)
        (fs-answer 1 (format "args contain non-string: ~s" string-args)))))


;; return spec for file operation
(define/contract (fs-answer errno [message (json-null)])
  (->* (integer?) (jsexpr?) jsexpr?)
  (hasheq 'errno errno
          'message message))

(define/contract (mkdir-op src)
  (-> string? void?)
  (define p (append-path webroot src))
  (log-rest-debug "mkdir-op ~a" p)
  (make-directory p))


(define/contract (rm-op src)
  (-> string? void?)
  (define p (append-path webroot src))
  (log-rest-debug "rm-op ~a" p)
  (delete-directory src))


(define/contract (mv-op src dst)
  (-> string? string? jsexpr?)
  (define src-full (append-path webroot src))
  (define dst-full (append-path webroot dst))
  (log-rest-debug "mv-op ~a ~a" src-full dst-full)
  (rename-file-or-directory src-full dst-full))


(define/contract (cat-op src data)
  (-> string? string? jsexpr?)
  (define p (append-path webroot src))
  (log-rest-debug "cat-op ~a [text of length ~d]" p (string-length data))
  (call-with-atomic-output-file p
    (lambda (out path)
      (display data out))))


(define/contract (ls-op src)
  (-> string? jsexpr?)
  (define p (append-path webroot src))
  (log-rest-debug "ls-op ~a" p)
  (map path->string (directory-list p)))
