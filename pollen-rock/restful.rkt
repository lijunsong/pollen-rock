#lang racket

(require web-server/http/request-structs)
(require json)
(require "config.rkt")
(require "util.rkt")
(require "http-util.rkt")
(require "logger.rkt")
(require (prefix-in pollen: pollen/file))
(require (prefix-in pollen: pollen/render))
(require (only-in "fs-watch.rkt" file-watch))
(require sugar)

(provide (all-defined-out))

;; To add more handlers for different route, add restful route in
;; main-handler, and define a sub-handler accepting request and
;; url-parts (path elements)
;;
;; NOTE: sub handlers must return a Jsexpr


;; main handler dispatch request to different handlers.
(define (main-handler req type url-parts)
  (print-request req)
  (define ans
    (match type
      ["fs" (fs-handler req url-parts (get-op-hash))]))
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
;; arguments. If it takes one argument, url-parts will be converted
;; to a resource and passed into the function. If two arguments,
;; `req` binding `data` will be passed as the second argument.
(define (fs-handler req url-parts op-hash)
  (define url-path (path-elements->resource url-parts))
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

;; when errno if 1, location must be false
(define/contract (render-answer errno location)
  (-> integer? (or/c false? string?) jsexpr?)
  (hasheq 'errno errno
          'location location))

;; take a matched url parts, render the file on disk.
;; renderer must return bool to indicate the success of rendering.
(define/contract (render-handler req url-parts renderer)
  (-> request? (listof string?) (-> string? boolean?) jsexpr?)
  (define url-path (path-elements->resource url-parts))
  (define source-path (append-path webroot url-path))
  (cond [(is-pollen-source? source-path)
         (with-handlers
             ([exn:fail? (lambda (e) (render-answer 1 false))])
           (if (renderer source-path)
               (render-answer 0 (path->string (pollen:->output-path url-path)))
               (render-answer 1)))]
        [else
         (render-answer 0 url-path)]))

(define/contract (handle-render source-path)
  (-> string? boolean?)
  (pollen:render-to-file-if-needed source-path)
  true)

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

;;;; POST that requires modules inspection

;; If variable is either char, string, or bool, val stores its value,
;; otherwise is (json-null).
(struct var-tag (name val) #:transparent)
;; proc-tag - stores proc information
(struct proc-tag (name arity kw) #:transparent)

(define/contract (extract-module-info modules)
  (-> (listof (or/c string? symbol? list?))
      (listof (or/c var-tag? proc-tag?)))
  (define (val->string v)
    (cond [(list? v)
           (map val->string v)]
          [(boolean? v) v]
          [else
           (with-handlers
               [(exn:fail? (lambda _ unknown-val))]
             (->string v))]))
  (define unknown-val (json-null))
  (define ns (make-base-empty-namespace))
  (for ([m modules])
    (with-handlers
        ([exn:fail? (lambda _ (void))])
      (parameterize ([current-namespace ns])
        (namespace-require m))))
  (define ids (namespace-mapped-symbols ns))
  (define vals  (map (lambda (s)
                       (namespace-variable-value
                        s true (lambda() unknown-val) ns))
                     ids))

  (map (lambda (name v)
         (if (procedure? v)
             (let ([arity (procedure-arity v)])
               (define-values (_ kws) (procedure-keywords v))
               (proc-tag name (if (arity-at-least? arity)
                                  (arity-at-least-value arity)
                                  arity)
                         kws))
             (var-tag name (val->string v))))
       ids
       vals))

;;;; POST /config/$path
