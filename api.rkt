#lang racket

;;; This file defines json object for an API request
;;;
;;; Attributes for common requests:
;;;
;;;   "type" the type of this API request.
;;;   "type" := autosave
;;;
;;; Attributes for autosave
;;;
;;;   "text"     : text to save
;;;   "resource" : resource (an absolute path) to save into
;;;
;;; Examples:
;;; - autosave: {type : "autosave",
;;;              resource : "/dir1/dir2/hello.html.pm",
;;;              text : "#lang pollen\n..."}

(require web-server/http)
(require web-server/http/bindings)

(require pollen/render) ;; handle render
(require pollen/file)   ;; handle render

(require "config.rkt")
(require "util.rkt")
(require "http-util.rkt")

(provide (combine-out api-post-handler))

;;; Structs for request APIs
(define (assert-resource resource)
  (unless (resource? resource)
    (error "Not a Resource: ~a" resource)))

(struct Autosave (resource text)
        #:transparent
        #:guard (lambda (f t tmp)
                  (assert-resource f)
                  (values f t)))

(struct Render (resource)
        #:transparent
        #:guard (lambda (f tmp)
                  (assert-resource f)
                  (values f)))

;;; Main handler for POST api request
;; TODO: To autosave a big file on a slow disk will cause problem.
;;       It seems what we need here is a producer-consumer queue.
(define/contract (api-post-handler req)
  (-> request? response?)
  (match (request-api-type req)
    ["autosave"
     (let ((autosave (request->autosave req)))
       (let ((saved? (handle-autosave autosave)))
         (response/text
          (if saved? #"saved" #"error occurs. Manually save your text"))))]
    ["render"
     (let* ((render (request->render req)))
       (handle-render render))]
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

;;; Autosave
(define (request->autosave req)
  (request->api-struct req Autosave 'resource 'text))

(define (handle-autosave autosave)
  (define filepath (append-path webroot (Autosave-resource autosave)))
  (cond [(not (file-exists? filepath)) #f]
        [else
         (call-with-output-file* filepath
           (lambda (out)
             (display (Autosave-text autosave) out))
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
