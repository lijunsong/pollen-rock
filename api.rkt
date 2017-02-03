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
(require "config.rkt")
(require "util.rkt")

(provide (combine-out api-post-handler
                      api-get-handler))

;;; Structs for APIs
(struct Autosave (resource text)
        #:transparent
        #:guard (lambda (f t tmp)
                  (unless (resource? f)
                    (error 'Autosave (format "Not a Resource: ~a" f)))
                  (values f t)))

;;; Main handler for POST api request
;; TODO: To autosave a big file on a slow disk will cause problem.
;;       It seems what we need here is a producer-consumer queue.
(define/contract (api-post-handler req)
  (-> request? response?)
  (match (request-api-type req)
    ["autosave"
     (let ((autosave (request->autosave req)))
       (let ((saved? (handle-autosave autosave)))
         (response/full
          200 #"Okay"
          (current-seconds)
          TEXT/HTML-MIME-TYPE
          empty
          (list (if saved? #"saved" #"error occurs. Manually save your text")))))]
    [x
     (response/xexpr
      `(html (head) (p ,(format "unknown POST command: ~a" x))))]))

;;; Main handler for GET api request
(define/contract (api-get-handler req)
  (-> request? response?)
  (response/xexpr
   `(html (head) (p ,(format "unknown GET command: ~a" req)))))

;;; Helper functions Below
(define (request-api-type req)
  (let ((bindings (request-bindings req)))
    (extract-binding/single 'type bindings)))

(define (request->autosave req)
  (let ((bindings (request-bindings req)))
    (let ((resource (extract-binding/single 'resource bindings))
          (text (extract-binding/single 'text bindings)))
      (Autosave resource text))))

;; lower helper function to hanlder autosave
(define (handle-autosave autosave)
  (define filepath (append-path webroot (Autosave-resource autosave)))
  (cond [(not (file-exists? filepath)) #f]
        [else
         (call-with-output-file* filepath
           (lambda (out)
             (display (Autosave-text autosave) out))
           #:mode 'text
           #:exists 'must-truncate)]))
