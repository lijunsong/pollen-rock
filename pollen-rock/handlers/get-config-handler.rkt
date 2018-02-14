#lang racket

(require json)
(require web-server/http/request-structs)
(require "expose-module.rkt")

(provide get-config-handler
         config-answer
         do-get-config)


;;;; GET /config/$path
;;
;; return tags are guarenteed to be a "variable" type
(define/contract (config-answer errno tags)
  (-> integer? (listof (hash/c symbol? jsexpr?)) jsexpr?)
  (hash 'errno errno
        'tags tags))


;; Getting pollen setup is a bit hairy here. One way to do it is to
;; inject some file inside the folder, and get setup from that file.
;; One obvious problem is the readonly filesystem (which will happen
;; when you mount NFS with readonly). Here let's just get everything
;; from pollen.rkt, which should cover 90% of usage, and still profit.
;; If anyone wants a more sophisticated system, we can always enhance
;; it later.
(define/contract (get-config-handler req url-parts config-getter)
  (-> request? (listof string?)
      (-> (or/c path-string? symbol?) (listof jsexpr?)) jsexpr?)
  (define setup-path (search-pollen/setup url-parts))
  (config-answer 0 (config-getter setup-path)))

;; find out the right pollen.rkt path for the given url.
;; This function also returns 'pollen/setup if no pollen.rkt is found
(define/contract (search-pollen/setup url-parts [found? file-exists?])
  (->* ((listof string?)) (procedure?) (or/c path? 'pollen/setup))
  (define setup-path-parts (append url-parts (list "pollen.rkt")))
  (define setup-path (apply build-path setup-path-parts))
  (cond [(found? setup-path) setup-path]
        [(empty? url-parts) 'pollen/setup]
        [else
         (search-pollen/setup (drop-right url-parts 1) found?)]))

;; get all bindings of pollen setup in the given module-path.
;; if an identifer appears multiple times, its final value is
;; the last value overwritten during requires
(define/contract (do-get-config module-path)
  (-> (or/c path-string? symbol?) (listof jsexpr?))
  (let  [(submodule (if (symbol? module-path)
                        module-path
                        `(submod ,module-path setup)))]
    (define config-json (extract-module-bindings
                         (list 'pollen/setup submodule)))
    (define (extract-var hash)
      (string=? (hash-ref hash 'kind) "variable"))

    (filter extract-var config-json)))



(module+ test
  (require rackunit)
  (require "../http-util.rkt")

  ;; unit test search-pollen/setup
  (check-equal?
   'pollen/setup
   (search-pollen/setup '("dir1" "dir2" "dir3") (lambda _ false)))

  (check-equal?
   (search-pollen/setup '("dir1" "dir2" "dir3")
                        (lambda (p) ;; pretend dir1/pollen.rkt is found
                          (equal? p (build-path "dir1"  "pollen.rkt"))))
   (build-path "dir1" "pollen.rkt"))

  (check-equal?
   (search-pollen/setup '("dir1" "dir2" "dir3")
                        (lambda (p) ;; pretend pollen.rkt is found
                          (equal? p (build-path "pollen.rkt"))))
   (build-path "pollen.rkt"))

  ;; GET config
  (define config-request-url-parts
    (list "path1" "path2" "test.html.pp"))
  (define config-request
    (make-test-request "config" config-request-url-parts
                       (make-hash)))

  (check-equal?
   (config-answer 0 empty)
   (get-config-handler
    config-request
    config-request-url-parts
    (lambda _
      (list))))


  )
