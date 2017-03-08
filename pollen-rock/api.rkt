#lang racket

;;; This file defines json object for an API request
;;;
;;; Attributes for common requests:
;;;
;;;   "type" the type of this API request.
;;;   "type" := save | render | config | shell
;;;
;;; Attributes for save
;;;   "text"     : text to save
;;;   "resource" : resource (an absolute path) to save into
;;;
;;; Attributes for shell
;;;   "cmd": command to run
;;;
;;;
;;; Examples:
;;; - save: {type : "save",
;;;          resource : "/dir1/dir2/hello.html.pm",
;;;          text : "#lang pollen\n..."}
#|

This file defines protocols between clients and server

|#

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

(provide api-post-handler)

;;; Structs for request APIs
(define (assert-resource resource)
  (unless (resource? resource)
    (error "Not a Resource: ~a" resource)))

;; request to save resource
(struct Save (resource text)
        #:transparent
        #:guard (lambda (f t tmp)
                  (assert-resource f)
                  (values f t)))

;; request to render the given resource
(struct Render (resource)
        #:transparent
        #:guard (lambda (f tmp)
                  (assert-resource f)
                  (values f)))

;; request pollen config of the given resource
(struct Config (resource)
        #:transparent
        #:guard (lambda (f tmp)
                  (assert-resource f)
                  (values f)))

;; request to run a shell command
(struct Shell (cmd) #:transparent)

;; request to watch file changes
;; seconds is the last modify seconds that the frontend knows about
;; this file
(struct WatchFile (resource seconds)
        #:transparent
        #:guard (lambda (f s tmp)
                  (assert-resource f)
                  (values f (string->number s))))

;; request user-defined tag information
(struct Tag (resource) #:transparent)
;; server uses this struct to organize info
;; if variable is char or string, val is the value, otherwise is #f
(struct VariableTag (name val) #:transparent)
(struct ProcedureTag (name arity keywords) #:transparent)

;;; Main handler for POST api request
;; TODO: To save a big file on a slow disk will cause problem.
;;       It seems what we need here is a producer-consumer queue.
(define/contract (api-post-handler req)
  (-> request? response?)
  (match (request-api-type req)
    ["save"
     (let ((save (request->save req)))
       (let ((saved? (handle-save save)))
         (response/text
          (if saved? #"saved" #"error occurs. Manually save your text"))))]
    ["render"
     (let ((render (request->render req)))
       (handle-render render))]
    ["config"
     (let ((config (request->config req)))
       (handle-config config))]
    ["shell"
     (let ((shell (request->shell req)))
       (handle-shell shell))]
    ["watchfile"
     (let ((wf (request->watchfile req)))
       (handle-watchfile wf))]
    [x
     (response/xexpr
      `(html (head) (p ,(format "unknown POST command: ~a" x))))]))

;;;; Helper functions
(define (request-api-type req)
  (let ((bindings (request-bindings req)))
    (extract-binding/single 'type bindings)))

(define/contract (request-extract-binding req ids)
  (-> request? (listof symbol?) (listof any/c))
  (define bindings (request-bindings req))
  (for/list ([id ids])
    (if (exists-binding? id bindings)
        (extract-binding/single id bindings)
        (error 'request-extract-binding "missing ~a in request" id))))

;; TODO: failure should be sent to client
(define (request->api-struct req strut . fields)
  (apply strut (request-extract-binding req fields)))

;;; Save
(define (request->save req)
  (request->api-struct req Save 'resource 'text))

;; save file in idemptent way
(define (handle-save save)
  (define filepath (append-path webroot (Save-resource save)))
  (cond [(not (file-exists? filepath)) #f]
        [else
         (call-with-atomic-output-file filepath
           (lambda (out path)
             (display (Save-text save) out)
             #t))]))


;;; Render
(define (request->render req)
  (request->api-struct req Render 'resource))

(define (handle-render render)
  (define resource (Render-resource render))
  (define file-path (path->complete-path
                     (append-path webroot resource)))

  (define answer
    (cond [(is-pollen-source? resource)
           (render-to-file-if-needed file-path)
           (resource->output-path resource)]
          [else resource]))

  (response/text answer))

;;; PollenConfig
(define (request->config req)
  (request->api-struct req Config 'resource))


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
(define (handle-config config)
  (define resource (Config-resource config))
  (define pollen-config
    (extract-module-info
     '(pollen/setup (submod "pollen.rkt" setup))))
  (define vars (filter VariableTag? pollen-config))
  (define pair (map (lambda (var)
                      (cons (VariableTag-name var)
                            (VariableTag-val  var))) vars))
  (define config-hash (hash-set* (make-immutable-hasheq pair)
                                'rendered-resource (resource->output-path resource)
                                'resource          resource))
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
  (define rock-hash (make-hash `((no-shell . ,(no-shell)))))
  (define tag-hash (make-immutable-hasheq tag-pair))
  (define ans-hash (make-immutable-hasheq
                    `((pollenConfig . ,config-hash)
                      (tags . ,tag-hash)
                      (rockConfig .  ,rock-hash))))
  (response/text (jsexpr->bytes ans-hash)))


;;; Shell Command
(define (request->shell req)
  (request->api-struct req Shell 'cmd))

(define (handle-shell shell)
  (define cmd (Shell-cmd shell))
  (response/text
   (cond [(no-shell)
          (display (format "[shell] disabled: command query '~a'\n" cmd))
          "disabled"]
         [else
          (call-with-output-bytes
           (lambda (p)
             (parameterize ([current-output-port p]
                            [current-error-port p])
               (system cmd))))])))

;;; WatchFile request
(define (request->watchfile req)
  (request->api-struct req WatchFile 'resource 'seconds))

(define (handle-watchfile wf)
  (define resource (WatchFile-resource wf))
  (define filepath (append-path webroot resource))
  (define last-seen-seconds (WatchFile-seconds wf))
  ;; TODO: send back file system's most recent modify seconds
  (define ans
    (make-hasheq `((rendered-resource . ,(resource->output-path resource))
                   (seconds . 0))))
  (define (handler _ last-mod)
    (hash-set! ans 'seconds last-mod))
  (when (file-watch filepath handler last-seen-seconds)
    (response/text (jsexpr->bytes ans))))
