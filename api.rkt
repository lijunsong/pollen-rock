#lang racket

;;; This file defines json object for an API request
;;;
;;; Attributes for common requests:
;;;
;;;   "type" the type of this API request.
;;;   "type" := save | render | config
;;;
;;; Attributes for save
;;;
;;;   "text"     : text to save
;;;   "resource" : resource (an absolute path) to save into
;;;
;;; Examples:
;;; - save: {type : "save",
;;;              resource : "/dir1/dir2/hello.html.pm",
;;;              text : "#lang pollen\n..."}

(require web-server/http)
(require web-server/http/bindings)

(require pollen/render) ;; handle render
(require pollen/file)   ;; handle render
(require json)

(require "config.rkt")
(require "util.rkt")
(require "http-util.rkt")

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
  (if (andmap (lambda (id)
                (exists-binding? id bindings))
              ids)
      (map (lambda (id)
             (extract-binding/single id bindings))
           ids)
      '()))

;; TODO: failure should be sent to client
(define (request->api-struct req strut . fields)
  (let ((xs (request-extract-binding req fields)))
    (if (empty? xs)
        (error "Incomplete request for ~a: ~a" strut req)
        (apply strut xs))))

;;; Save
(define (request->save req)
  (request->api-struct req Save 'resource 'text))

(define (handle-save save)
  (define filepath (append-path webroot (Save-resource save)))
  (cond [(not (file-exists? filepath)) #f]
        [else
         (call-with-output-file* filepath
           (lambda (out)
             (display (Save-text save) out))
           #:mode 'text
           #:exists 'must-truncate)]))

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

(define config-ids
  '(command-char render-cache-active compile-cache-active))

(define config-setup:ids
  (map (lambda (id)
         (string->symbol
          (string-append "setup:" (symbol->string id))))
       config-ids))

;; to load user's pollen.rkt, either define
;; `current-load-relative-directory`, or sevlet defines
;; `current-directory` to user's working directory.
(define (handle-config config)
  (define resource (Config-resource config))
  (define spacen (make-base-namespace))
  (eval '(require pollen/setup) spacen)
  ;(eval `(current-load-relative-directory ,webroot) spacen)
  (eval `(require (submod "pollen.rkt" setup)) spacen)

      ;; get config info from pollen setup
  (define vs
    (map (lambda (n setup:n)
           (let [(v (eval `(,setup:n) spacen))]
             (cons n
                   (if (char? v) (string v)
                       v))))
         config-ids config-setup:ids))
  (let* [(hash (make-immutable-hasheq vs))
         (v (hash-set hash
                      'rendered-resource
                      (resource->output-path resource)))]
    (response/text (jsexpr->bytes v))))

#|
(define webroot "/Users/ljs/workspace/pollen-test")
(define pname (append-path webroot "pollen.rkt"))
(define spacen (make-base-namespace))

;(eval '(load "/Users/ljs/workspace/pollen-test/pollen.rkt") spacen)
(eval `(begin
         (current-load-relative-directory "/Users/ljs/workspace/pollen-test")
         (require (submod "pollen.rkt" setup))
         ;(require pollen/setup)
         ;(setup:command-char)
         (identifier-binding #'command-charx)
         ;(dynamic-require '(submod "pollen.rkt" setup) 'command-char)
         )
      spacen)

;(eval `(namespace-require (require (file ,pname))) spacen)
|#
