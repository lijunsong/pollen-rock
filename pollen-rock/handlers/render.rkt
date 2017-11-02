#lang racket

(require json)
(require net/url-string)
(require (prefix-in pollen: pollen/file))
(require (prefix-in pollen: pollen/render))
(require web-server/http/request-structs)

(require "../http-util.rkt")

;;;; POST /render/$path

;; when errno if 1, location must be false
(define/contract (render-answer errno location)
  (-> integer? (or/c false? string?) jsexpr?)
  (hasheq 'errno errno
          'location location))

;; take a matched url parts, render the file on disk.
;; renderer must return bool to indicate the success of rendering.
(define/contract (render-handler req url-parts renderer)
  (-> request? (listof string?) (-> path? boolean?) jsexpr?)
  (define source-path (apply build-path url-parts))
  (define output-path (pollen:->output-path source-path))
  (define output-url (relative-path->relative-url-string output-path))
  (if (equal? source-path output-path)
      (render-answer 0 output-url)
      (with-handlers
          ([exn:fail? (lambda (e) (render-answer 1 false))])
        (if (renderer source-path)
            (render-answer 0 output-url)
            (render-answer 1 false)))))

(define/contract (handle-render source-path)
  (-> path? boolean?)
  (pollen:render-to-file-if-needed source-path)
  true)


(module+ test
  (require rackunit)

  ;; test render-handler
  (define render-pm-url-parts (list "blog" "1.html.pm"))
  (define render-pm-request
    (make-test-request "render" render-pm-url-parts
                       (hash)))

  ;; test render-handler passes the correct source-path
  (let [(source-path (make-parameter "unknown"))]
    (check-equal?
     (render-answer 0 "blog/1.html")
     (render-handler render-pm-request
                     render-pm-url-parts
                     (lambda (src-path)
                       (source-path src-path)
                       true)))
    (check-equal? (source-path) (string->path "blog/1.html.pm")))

  ;; render-handler returns source path directory because
  ;; the rendering target is not a pollen source
  (define render-html-url-parts (list "blog" "2.html"))
  (define render-html-request
    (make-test-request "render" render-html-url-parts (hash)))

  (check-equal?
   (render-answer 0 "blog/2.html")
   (render-handler render-html-request
                   render-html-url-parts
                   (lambda (src-path)
                     (raise (make-exn:fail "s"
                                           (current-continuation-marks))))))

  ;; TODO: test render-handler handles correct path format in Windows

  ;; test render-handler returns jsexp when renderer throws exceptions
  (check-equal?
   (render-answer 1 false)
   (render-handler
    render-pm-request
    render-pm-url-parts
    (lambda (source-path)
      (raise (make-exn:fail:filesystem:exists "s"
                                              (current-continuation-marks))))))


  )
