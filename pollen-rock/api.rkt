#lang racket

;;; This file defines json object for API requests

(require web-server/http)
(require web-server/http/bindings)

(require pollen/render) ;; handle render
(require pollen/file)   ;; handle render
(require json)
(require sugar)

(require "config.rkt")
(require "util.rkt")
(require "http-util.rkt")
(require "fs-watch.rkt")
(require "libs/rpc.rkt")

(provide api-post-handler)


;; request user-defined tag information
(struct Tag (resource) #:transparent)
;; server uses this struct to organize info
;; if variable is char or string, val is the value, otherwise is #f
(struct VariableTag (name val) #:transparent)
(struct ProcedureTag (name arity keywords) #:transparent)


;; TODO: To save a big file on a slow disk will cause problem.
;;       It seems what we need here is a producer-consumer queue.
(define (save-handler text resource)
  (define filepath (append-path webroot resource))
  (cond [(not (file-exists? filepath)) #f]
        [else
         (call-with-atomic-output-file filepath
           (lambda (out path)
             (display text out)
             #t))]))

(define (render-handler resource)
  (define file-path (path->complete-path
                     (append-path webroot resource)))
  (cond [(is-pollen-source? resource)
         (render-to-file-if-needed file-path)
         (resource->output-path resource)]
        [else resource]))

;; seconds is the last modify seconds that the frontend knows about
;; this file
(define (watchfile-handler resource last-seen-seconds)
  (define filepath (append-path webroot resource))
  (define ans
    (make-hasheq `((rendered-resource . ,(resource->output-path resource))
                   (seconds . 0))))
  (define (handler _ last-mod)
    (hash-set! ans 'seconds last-mod))
  (when (file-watch filepath handler last-seen-seconds)
    ans))


(define/contract (extract-module-info modules)
  (-> (listof (or/c string? symbol? list?))
      (listof (or/c VariableTag?
                    ProcedureTag?)))
  (define unknown-val "unknown")
  (define ns (make-base-empty-namespace))
  (for ([m modules])
    (with-handlers
        ([exn:fail? (lambda _ (void))])
      (parameterize ([current-namespace ns])
        (namespace-require m))))
  (define ids (namespace-mapped-symbols ns))
  (define vals  (map (lambda (s)
                       (namespace-variable-value
                        s #t (lambda() unknown-val) ns))
                     ids))
  (define (val->string v)
    (cond [(list? v)
           (map val->string v)]
          [(boolean? v) (if v "true" "false")]
          [else
           (with-handlers
               [(exn:fail? (lambda _ unknown-val))]
             (->string v))]))
  (map (lambda (name v)
         (if (procedure? v)
             (let ([arity (procedure-arity v)])
               (define-values (_ kws) (procedure-keywords v))
               (ProcedureTag
                name
                (if (arity-at-least? arity)
                    (arity-at-least-value arity)
                    arity)
                kws))
             (VariableTag name (val->string v))))
       ids
       vals))

;; to load user's pollen.rkt, either define
;; `current-load-relative-directory`, or sevlet defines
;; `current-directory` to user's working directory.
(define (get-project-config-handler resource)
  (define pollen-config
    (extract-module-info
     '(pollen/setup (submod "pollen.rkt" setup))))
  (define vars (filter VariableTag? pollen-config))
  (define setup (map (lambda (var)
                      (cons (VariableTag-name var)
                            (VariableTag-val  var))) vars))
  (define pollen-tags
    (extract-module-info '("pollen.rkt")))
  (define tag-pair (map (lambda (v)
                          (cond [(VariableTag? v)
                                 (cons (VariableTag-name v) (VariableTag-val v))]
                                [(ProcedureTag? v)
                                 (cons (ProcedureTag-name v)
                                       (make-immutable-hasheq
                                        (list (cons 'arity (ProcedureTag-arity v))
                                              (cons 'keywords (ProcedureTag-keywords v)))))]
                                [else (error "handle config request error: unknown tag type")]))
                        pollen-tags))
  (hasheq 'setup (make-hasheq setup)
          #|(hasheq 'rendered-resource (resource->output-path resource)
                  'resource resource)|#
          'tags (make-immutable-hasheq tag-pair)))

;;; Main handler for POST api request
(define api-post-handler
  (export-rpc-handler
   (hash #"save" save-handler
         #"render" render-handler
         #"get-project-config" get-project-config-handler
         #"watchfile" watchfile-handler)))
