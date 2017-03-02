#lang racket

(require rackunit)
(require (prefix-in pollen: pollen/file))

(provide (all-defined-out))

(define (append-path . args)
  (define (to_string p)
    (if (path? p) (path->string p) p))
  (let ((p (string-join (map to_string args) "/")))
    (path->string (simplify-path p #f))))

(check-equal? (append-path "/12" "2") "/12/2")
(check-equal? (append-path "/12" "" "3") "/12/3")
(check-equal? (append-path "/1" "/2" "/3") "/1/2/3")
(check-equal? (append-path "/1" "/2" "/3/") "/1/2/3/")
(check-equal? (append-path "1" "2" "3") "1/2/3")

;; resource is web application specified path, and thus always has
;; prefix /
(define/contract (resource? r)
  (-> string? boolean?)
  (string-prefix? r "/"))

;; assemble url component into a resource
(define/contract (string-list->resource lst)
  (-> (listof string?) resource?)
  (string-append "/" (string-join lst "/")))

;; input: /a/b/c
;; output: ("/a" "/a/b" "/a/b/c")
(define/contract (resource->breadcrumb-url resource)
  (-> resource? (listof resource?))

  (define/contract (resource-component-url res rev-url-list)
    (-> (listof string?) (listof resource?) (listof resource?))
    (cond ((empty? res) (rest (reverse rev-url-list)))
          (else
           (let ((rev-url (append-path (first rev-url-list)
                                       (first res))))
             (resource-component-url (rest res) (cons rev-url rev-url-list))))))

  (resource-component-url (string-split resource "/") (list "/")))

(check-equal? (resource->breadcrumb-url "/") (list))
(check-equal? (resource->breadcrumb-url "/a") (list "/a"))
(check-equal? (resource->breadcrumb-url "/a/b/c") (list "/a" "/a/b" "/a/b/c"))


(define/contract (resource->output-path resource)
  (-> resource? string?)
  (path->string (pollen:->output-path resource)))

(define/contract (is-pollen-source? resource)
  (-> resource? boolean?)
  (not (string=? (resource->output-path resource) resource)))