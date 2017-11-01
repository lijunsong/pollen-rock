#lang racket

(require (prefix-in pollen: pollen/file))
(require sugar)

(provide (all-defined-out))





(define (append-path . args)
  (define (to-relative p)
    (cond ([equal? p ""] ".")
          ([absolute-path? p]
           (string-append "." (->string p)))
          (else p)))
  (cond ([empty? args] args)
        ([empty? (rest args)] (->string
                               (simplify-path (first args) #f)))
        (else
         (define new-path
           (apply build-path
                  (cons (first args)
                        (map to-relative (rest args)))))
         (->string (simplify-path new-path #f)))))

;; resource is web application specified path, and thus always has
;; prefix / (platform independent)
(define/contract (resource? r)
  (-> string? boolean?)
  (string-prefix? r "/"))

;;; check if path is in folder
(define/contract (is-in-folder? path folder [strict false])
  (->* (complete-path? complete-path?) (boolean?) boolean?)
  (define s-path (append-path path "."))
  (define s-folder (append-path folder "."))
  (if ((if strict <= <) (string-length s-path) (string-length s-folder))
      #f
      (string=? s-folder (substring s-path 0 (string-length s-folder)))))

;; take each path element out
(define/contract (resource->path-elements r)
  (-> resource? (listof string?))
  (map path->string (rest (explode-path r))))

;; assemble url component into a resource
(define/contract (path-elements->resource lst)
  (-> (listof string?) resource?)
  (string-append "/" (string-join lst "/")))

;; input: /a/b/c => ("/a" "/a/b" "/a/b/c")
;; input: / => ()
;; Note: this function returns url, so it's platform independent
(define/contract (resource->breadcrumb-url resource)
  (-> resource? (listof resource?))

  (define components (map ->string (rest (explode-path resource))))
  (define accumulated (for/list ([i (in-range 1 (+ 1 (length components)))])
                        (take components i)))
  (map path-elements->resource accumulated))


(define/contract (resource->output-path resource)
  (-> resource? string?)
  (path->string (pollen:->output-path resource)))

(define/contract (is-pollen-source? resource)
  (-> resource? boolean?)
  (not (string=? (resource->output-path resource) resource)))

(module+ test
  (require rackunit)
  (define (check-path-equal? p1 p2)
    (check-equal? (normal-case-path p1)
                  (normal-case-path p2)))

  (check-equal? (resource->path-elements "/") '())
  (check-equal? (resource->path-elements "/1") '("1"))
  (check-equal? (resource->path-elements "/1/2/3/") '("1" "2" "3"))
  (check-equal? (resource->path-elements "/1/2/3") '("1" "2" "3"))

  (check-equal? (path-elements->resource '()) "/")
  (check-equal? (path-elements->resource '("1")) "/1")
  (check-equal? (path-elements->resource '("1" "2")) "/1/2")

  (check-equal? (resource->output-path "/1.html.pp") "/1.html")
  (check-equal? (resource->output-path "/1.html.pmd") "/1.html")
  (check-equal? (resource->output-path "/1.html.p") "/1.html")
  (check-equal? (resource->output-path "/1.html.pm") "/1.html")

  (check-true (is-in-folder? "/a/b/c" "/a/b/c"))
  (check-false (is-in-folder? "/a/b/c" "/a/b/c" true))
  (check-true (is-in-folder? "/a/b/c/" "/a/b/c"))
  (check-false (is-in-folder? "/a/b/c/" "/a/b/c" true))

  (check-false (is-in-folder? "/a/b/" "/a/b/c"))
  (check-true (is-in-folder? "/a/b/c/d" "/a/b/c"))
  (check-true (is-in-folder? "/a/b/c/d/e/f/g" "/a/b/c"))
  (check-true (is-in-folder? "/a" "/"))

  (check-false (is-in-folder? "/a/b/c/.." "/a/b/c"))
  (check-false (is-in-folder? "/a/b/c/d/e/../../.." "/a/b/c"))



  (check-true (is-pollen-source? "/1.html.pp"))
  (check-true (is-pollen-source? "/1.html.pm"))
  (check-true (is-pollen-source? "/1.html.pmd"))
  (check-true (is-pollen-source? "/1.html.p"))
  (check-false (is-pollen-source? "/1.html"))

  (check-equal? (resource->breadcrumb-url "/") (list))
  (check-equal? (resource->breadcrumb-url "/a")
                '("/a"))
  (check-equal? (resource->breadcrumb-url "/a/b/c")
                '("/a" "/a/b" "/a/b/c"))

  (check-path-equal? (append-path "/12" "2") "/12/2")
  (check-path-equal? (append-path "/12" ".." ".." ".." "..") "/")
  (check-path-equal? (append-path "/12" "" "3") "/12/3")
  (check-path-equal? (append-path "/12" ".." "3") "/3")
  (check-path-equal? (append-path "12" ".." "3") "3")
  (check-path-equal? (append-path "12" "." "3") "12/3")
  (check-path-equal? (append-path "/12" "." "3") "/12/3")
  (check-path-equal? (append-path "/1" "/2" "/3") "/1/2/3")
  (check-path-equal? (append-path "/1" "/2" "/3/") "/1/2/3/")
  (check-path-equal? (append-path "1" "2" "3") "1/2/3")
  )
