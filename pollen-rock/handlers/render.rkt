#lang racket

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


(module+ test
  (require rackunit)

;;;; test render-handler
(define render-url-parts (list "blog" "1.html.pm"))
(define render-request
  (make-test-request "render" render-url-parts
                     (hash)))

;; test render-handler passes the correct source-path
(check-equal?
 (render-answer 0 "/blog/1.html")
 (render-handler
  render-request
  render-url-parts
  (lambda (source-path)
    (check-equal? source-path (append-path webroot "/blog/1.html.pm"))
    true)))

;; test render-handler returns jsexp when renderer throws exceptions
(check-equal?
 (render-answer 1 false)
 (render-handler
  render-request
  render-url-parts
  (lambda (source-path)
    (raise (make-exn:fail:filesystem:exists "s" (current-continuation-marks))))))


  )
