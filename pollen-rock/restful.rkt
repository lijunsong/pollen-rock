#lang racket

(require web-server/http/request-structs)
(require json)
(require "config.rkt")
(require "util.rkt")
(require "http-util.rkt")
(require "logger.rkt")

(provide (all-defined-out))

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
      ["fs" (fs-handler req matched-url (get-op-hash))]))
  ;;; convert ans to jsexp
  (response/text (jsexpr->bytes ans)))


(define/contract (extract-bindings key bindings)
  (-> bytes? (listof binding?) (or/c false? bytes?))
  (let [(form (bindings-assq key bindings))]
    (if form
        (binding:form-value form)
        false)))


;;;; POST /fs/$path
(define (get-op-hash)
  (hash #"mv" mv-op
        #"mkdir" mkdir-op
        #"rm" rm-op
        #"echo" echo-op
        #"ls" ls-op))


;; select matched handler from `handler-map` to respond to the `req`.
;; the value of handler-map must be taking one or two positional
;; arguments. If it takes one argument, matched-url will be converted
;; to a resource and passed into the function. If two arguments,
;; `req` binding `data` will be passed as the second argument.
(define (fs-handler req matched-url op-hash)
  (define url-path (path-elements->resource matched-url))
  (define bindings (request-bindings/raw req))
  (define fs-keys (list #"op" #"data"))
  (define binding-assoc (map (lambda (k)
                              (cons k
                                    (extract-bindings k bindings)))
                             fs-keys))
  (define binding-hash (make-hash binding-assoc))
  (handle-filesystem-op url-path binding-hash op-hash))


(define (handle-filesystem-op url-path binding-hash op-hash)
  (define op (hash-ref op-hash
                       (hash-ref binding-hash #"op" false)
                       false))
  (define data (hash-ref binding-hash #"data" false))
  (cond [(not op)
         (fs-answer 1 "unknown op")]
        [(= (procedure-arity op) 2)
         ((op-sanity-checker op) url-path data)]
        [(= (procedure-arity op) 1)
         ((op-sanity-checker op) url-path)]
        [else
          (fs-answer 1 "internal error. arity isn't 1 or 2")]))


;; Sanity check all arguments passed to the given function
;;
;; TOOD: define our own exn here and let func throws exception (fs
;; exception, our own exception, and catch those exceptions here)
(define (op-sanity-checker func)
  (define (arg->string arg)
    (if (bytes? arg)
        (bytes->string/utf-8 arg)
        arg))
  (define (fs-op-with-handlers op)
    (lambda args
      (with-handlers
          ([exn:fail:filesystem? (lambda (e) (fs-answer 1 (exn-message e)))])
        (let [(ret (apply op args))]
          (if (void? ret)
              (fs-answer 0)
              (fs-answer 0 ret))))))
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
  (-> string? string? void?)
  (define src-full (append-path webroot src))
  (define dst-full (append-path webroot dst))
  (log-rest-debug "mv-op ~a ~a" src-full dst-full)
  (rename-file-or-directory src-full dst-full))


(define/contract (echo-op src data)
  (-> string? string? void?)
  (define p (append-path webroot src))
  (log-rest-debug "echo-op ~a [text of length ~s]" p (string-length data))
  (call-with-atomic-output-file p
    (lambda (out path)
      (display data out))))


(define/contract (ls-op src)
  (-> string? jsexpr?)
  (define p (append-path webroot src))
  (log-rest-debug "ls-op ~a" p)
  (map path->string (directory-list p)))



;;;; POST /render/$path
#|
(define/contract (render-answer errno)
  (-> ... jsexpr?)
  (hasheq 'errno errno))


(define (render-handler req matched-url)
  (define filepath (append-path (path-elements->resource matched-url)))
  (cond [(is-pollen-source? filepath)
         (render-to-file-if-needed filepath)
         (render-answer 0)]
        [else
         (render-answer 1)]))
|#
