#lang racket

;;; API: run filesystem operations


(require "../http-util.rkt")
(require "../logger.rkt")
(require json)
(require web-server/http/request-structs)

(provide fs-handler get-op-hash)


;; return spec for file operation
(define/contract (fs-answer errno [message (json-null)])
  (->* (integer?) (jsexpr?) jsexpr?)
  (hasheq 'errno errno
          'message message))

;; select matched handler from `handler-map` to respond to the `req`.
;; the value of handler-map must be taking one or two positional
;; arguments. If it takes one argument, url-parts will be converted to
;; a path and passed into the function. If two arguments, `req`
;; binding `data` will be passed as bytes to the second argument.
(define (fs-handler req url-parts op-hash)
  (define url-path (apply build-path url-parts))
  (define bindings (request-bindings/raw req))
  (define fs-keys (list #"op" #"data"))
  (define binding-assoc (map (lambda (k)
                              (cons k
                                    (extract-bindings k bindings)))
                             fs-keys))
  (define binding-hash (make-hash binding-assoc))
  (handle-filesystem-op url-path binding-hash op-hash))


(define/contract (handle-filesystem-op url-path binding-hash op-hash)
  (-> (and/c path? relative-path?) (hash/c bytes? (or/c false? bytes?))
      (hash/c bytes? procedure?) jsexpr?)
  (define opname (hash-ref binding-hash #"op" false))
  (define op (hash-ref op-hash opname false))
  (define data (hash-ref binding-hash #"data" false))
  (cond [(not op)
         (fs-answer 1 "unknown op")]
        [(and (= (procedure-arity op) 2) (not data))
         (fs-answer 1 (format "~a: data parameter not found" opname))]
        [(= (procedure-arity op) 2)
         ;; data sometimes is a path, sometimes is real data.
         ;; let ops decide whether to convert bytes to path
         ((op-sanity-checker op) url-path data)]
        [(= (procedure-arity op) 1)
         ((op-sanity-checker op) url-path)]
        [else
          (fs-answer 1 "internal error. arity isn't 1 or 2")]))


;; Sanity check all arguments passed to the given function
;;
;; TOOD: define our own exn here and let func throws exception (fs
;; exception, our own exception, and catch those exceptions here)
(define (op-sanity-checker func)
  (define (fs-op-with-handlers op)
    (lambda args
      (with-handlers
          ([exn:fail:filesystem? (lambda (e) (fs-answer 1 (exn-message e)))])
        (let [(ret (apply op args))]
          (if (void? ret)
              (fs-answer 0)
              (fs-answer 0 ret))))))
  (lambda (src-path . data)
    (cond [(not (relative-path? src-path))
           (fs-answer 1 (format "src must be relative path: ~s" src-path))]
          [else
           (apply (fs-op-with-handlers func) (cons src-path data))])))


(define/contract (mkdir-op src)
  (-> relative-path? void?)
  (log-rest-debug "mkdir-op ~a" src)
  (make-directory src))


(define/contract (rm-op src)
  (-> relative-path? void?)
  (log-rest-debug "rm-op ~a" src)
  (delete-directory src))


(define/contract (mv-op src data)
  (-> relative-path? bytes? void?)
  (define dst (bytes->path data))
  (log-rest-debug "mv-op ~a ~a" src dst)
  (rename-file-or-directory src dst))


(define/contract (echo-op src data)
  (-> relative-path? bytes? void?)
  (log-rest-debug "echo-op ~a [text of length ~s]" src (string-length data))
  (call-with-atomic-output-file src
    (lambda (out path)
      (display data out))))

(define/contract (ls-op src)
  (-> relative-path? jsexpr?)
  (log-rest-debug "ls-op ~a" src)
  (map path->string (directory-list src)))


;;;; POST /fs/$path
(define (get-op-hash)
  (hash #"mv" mv-op
        #"mkdir" mkdir-op
        #"rm" rm-op
        #"echo" echo-op
        #"ls" ls-op))

(module+ test
  (require rackunit)

  ;; helper check functions
  (define (check-fs-answer-errno-equal? a b)
    (check-equal? (hash-ref a 'errno) (hash-ref b 'errno)))


  ;; test handle-fileystem-op

  ;; test it will return fs-answer when bindings are missing
  (check-fs-answer-errno-equal?
   (fs-answer 1 "")
   (handle-filesystem-op
    (string->path "hello/go")
    (hash #"op" #"mv")
    (hash #"mv" (lambda (src dst)
                  (void)))))

  (check-fs-answer-errno-equal?
   (fs-answer 1 "")
   (handle-filesystem-op
    (string->path "hello/go")
    (hash #"unknown" #"unknown")
    (hash #"mv" (lambda (src dst)
                  (void)))))

  ;; test it will pass the correct parameter inside
  (check-fs-answer-errno-equal?
   (fs-answer 0)
   (handle-filesystem-op
    (string->path "src")
    (hash #"op" #"myop" #"data" #"second-arg")
    (hash #"myop" (lambda (src)
                    (check-equal? src
                                  (string->path "src"))))))

  (check-fs-answer-errno-equal?
   (fs-answer 0)
   (handle-filesystem-op
    (string->path "src")
    (hash #"op" #"myop" #"data" #"second-arg\1")
    (hash #"myop" (lambda (src dst)
                    (check-equal? src (string->path "src"))
                    (check-equal? dst #"second-arg\1")))))

  ;; test whether handle-filesystem-op would return what the actual op
  ;; returns
  (check-equal?
   (fs-answer 0 (list "1" "2"))
   (handle-filesystem-op
    (string->path "src")
    (hash #"op" #"ls")
    (hash #"ls" (lambda (src)
                  (list "1" "2")))))

  ;; test whether handle-filesystem-op would return fs-answer when
  ;; the actual op throws exceptions
  (check-equal?
   (fs-answer 1 "s")
   (handle-filesystem-op
    (string->path "src")
    (hash #"op" #"ls")
    (hash #"ls" (lambda (src)
                  (raise (make-exn:fail:filesystem
                          "s"
                          (current-continuation-marks)))))))

  ;; test handle-filesystem-op always returns error 1
  (check-equal?
   (fs-answer 1 "s")
   (handle-filesystem-op
    (string->path "src")
    (hash #"op" #"ls")
    (hash #"ls" (lambda (src)
                  (raise (make-exn:fail:filesystem:errno
                          "s"
                          (current-continuation-marks)
                          (cons 129 'windows)))))))

  ;; test fs-handler
  ;; exercise the full path from accepting request to handle request
  (define mv-request-url-parts (list "src" "path1"))
  (define mv-request
    (make-test-request "fs" mv-request-url-parts
                       (hash #"op" #"mv"
                             #"data" #"/dst/path2")))

  (check-equal?
   (fs-handler mv-request mv-request-url-parts
               (hash #"mv" (lambda (src dst)
                             (check-equal? src (build-path "src" "path1"))
                             ;; dst here is bytes!
                             (check-equal? dst #"/dst/path2"))))
   (fs-answer 0))


  )
