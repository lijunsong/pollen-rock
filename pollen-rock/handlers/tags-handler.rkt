#lang racket

(require json)
(require web-server/http/request-structs)
(require "expose-module.rkt")

(provide tags-answer
         tags-handler
         do-get-tags)

(define/contract (tags-answer errno tags)
  (-> integer? (listof (hash/c symbol? jsexpr?)) jsexpr?)
  (hash 'errno errno
        'tags tags))


(define/contract (tags-handler req url-parts tags-getter)
  (-> request? (listof string?)
      (-> path-string? (listof jsexpr?)) jsexpr?)
  (define mod-path (apply build-path url-parts))
  (tags-answer 0 (tags-getter mod-path)))

;; TODO: make extract-module-bindings take non-list and return
;; another value to indicate if extraction succeeds.
(define/contract (do-get-tags mod-path)
  (-> path-string? (listof jsexpr?))
  (extract-module-bindings (list mod-path)))


(module+ test
  (require rackunit)
  (require "../http-util.rkt")

  (define tags-request-url-parts
    (list "path1" "path2" "file.html.pm"))
  (define tags-request
    (make-test-request "tags" tags-request-url-parts
                       (hash)))

  ;; test that handler passes the correct path into getter
  (check-equal?
   (tags-answer 0 (list (hash 'x 1)))
   (tags-handler
    tags-request tags-request-url-parts
    (lambda (mod-path)
      (check-equal?
       mod-path
       (apply build-path tags-request-url-parts))
      (list (hash 'x 1)))))
  )
