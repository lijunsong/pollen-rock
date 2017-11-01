#lang racket

;;;; GET /config/$path
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
      (-> path-string? (listof jsexpr?)) jsexpr?)
  (define setup-path (get-pollen-setup-path url-parts))
  (config-answer 0 (config-getter setup-path)))

;; find out the right pollen.rkt path for the given url.
;; This function also returns 'pollen/setup if no pollen.rkt is found
;; TODO: make this take a predicate to swap file-exists.
(define/contract (get-pollen-setup-path url-parts)
  (-> (listof string?) (or/c path-string? 'pollen/setup))
  (define setup-path-parts
    (cons webroot (append url-parts (list "pollen.rkt"))))
  (define setup-path (apply build-path webroot))
  (cond [(file-exists? setup-path) setup-path]
        [(empty? url-parts) 'pollen/setup]
        [else
         (get-pollen-setup-path (drop-right url-parts 1))]))

(define/contract (get-config module-path)
  (-> path-string? (listof jsexpr?))
  (define config-json (extract-module-bindings module-path))
  config-json)


(module+ test
;;;; GET config
(define config-request-url-parts
  (list "path1" "path2" "test.html.pp"))
(define config-request
  (make-test-request "config" config-request-url-parts
                     (make-hash)))

(check-equal?
 (config-answer 0 empty)
 (get-config-handler config-request config-request-url-parts
                     (lambda (module-path)
                       (check-equal? module-path
                                     (apply build-path
                                            (reverse
                                             (cons INJECT-CONFIG-FILENAME
                                                   (rest (reverse config-request-url-parts))))))
                       empty)))


  )
