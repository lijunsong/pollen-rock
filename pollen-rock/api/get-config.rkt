#lang racket

(require json)
(require sugar)

(provide (all-defined-out))

;;; API: get pollen config

;; produce a json to describe a procedure
;; name: (or/c string? (json-null))
;; kind: "procedure"
;; arity: (or/c integer? (listof integer?))
;; arity-at-least: boolean?
;; required-keywords: (listof string?)
;; all-keywords: (or/c (listof string?) #f)
;;       where #f means any keyword is accepted
;;
;; Note it is when proc takes any number of arguments (like
;; case-lambda) when arity is a list,
(define/contract (procedure->json proc)
  (-> procedure? jsexpr?)
  (define name (object-name proc))
  (define arity (procedure-arity proc))
  (define-values (required-keywords all-keywords)
    (procedure-keywords proc))
  (hasheq 'name (if name (symbol->string name) (json-null))
          'kind "procedure"
          'arity (if (arity-at-least? arity)
                     (arity-at-least-value arity)
                     arity)
          'arity-at-least (arity-at-least? arity)
          'required-keywords (map keyword->string required-keywords)
          'all-keywords
          (if all-keywords
              (map keyword->string all-keywords)
              #f)))

(define/contract (var->json name val)
  (-> string? any/c jsexpr?)
  (define checks (list (list boolean? identity)
                       (list number? identity)
                       (list string? identity)
                       (list symbol? identity)
                       (list struct? (lambda _ (json-null)))
                       ;; the last one always succeeds
                       (list (lambda _ #t) (lambda _ (json-null)))))
  (define (remove-? symbol)
    (let [(s (symbol->string symbol))]
      (string-trim s #\? #:right? true)))
  (define (identify current-check all-checks)
    (let [(identify-type (first current-check))
          (identify-val  (second current-check))]
      (if (identify-type val)
          (hasheq 'name name
                  'kind "variable"
                  'value (identify-val val)
                  'type (remove-? (object-name identify-type)))
          (identify (first all-checks) (rest all-checks)))))
  (identify (first checks) (rest checks)))

(define/contract (extract-module-info modules)
  (-> (listof (or/c string? symbol? list?))
      (listof jsexpr?))
  (define ns (make-base-empty-namespace))
  (for ([m modules])
    (with-handlers
        ([exn:fail? (lambda _ (void))])
      (parameterize ([current-namespace ns])
        (namespace-require m))))
  (define ids (namespace-mapped-symbols ns))
  (define vals  (map (lambda (s)
                       (namespace-variable-value
                        s true (lambda _ (json-null)) ns))
                     ids))

  (map (lambda (name v) (if (procedure? v)
                            (procedure->json v)
                            (var->json name v))) ids vals))

(module+ test
  (require rackunit)
  (require syntax/location)

  ;; test procedure json conversion
  (define (proc0 a b c) 1)
  (check-equal? (procedure->json proc0)
                (hasheq 'name "proc0"
                        'kind "procedure"
                        'arity 3
                        'arity-at-least false
                        'required-keywords empty
                        'all-keywords empty))

  (define (proc1 a b . c) 1)
  (check-equal? (procedure->json proc1)
                (hasheq 'name "proc1"
                        'kind "procedure"
                        'arity 2
                        'arity-at-least true
                        'required-keywords empty
                        'all-keywords empty))

  (define (proc2 a #:kw1 kwarg1 #:kw2 [kwarg2 1]) 1)
  (check-equal? (procedure->json proc2)
                (hasheq 'name "proc2"
                        'kind "procedure"
                        'arity 1
                        'arity-at-least false
                        'required-keywords '("kw1")
                        'all-keywords '("kw1" "kw2")))

  (define (proc3 a #:kw1 [kwarg1 0] #:kw2 [kwarg2 1]) 1)
  (check-equal? (procedure->json proc3)
                (hasheq 'name "proc3"
                        'kind "procedure"
                        'arity 1
                        'arity-at-least false
                        'required-keywords '()
                        'all-keywords '("kw1" "kw2")))

  (define proc4
    (case-lambda [(x) 0]
                 [(x y) 1]))
  (check-equal? (procedure->json proc4)
                (hasheq 'name "proc4"
                        'kind "procedure"
                        'arity (list 1 2)
                        'arity-at-least false
                        'required-keywords empty
                        'all-keywords empty))

  (define proc5
    (case-lambda [(x) 0]
                 [(x y) 1]
                 [r r]))
  (check-equal? (procedure->json proc5)
                (hasheq 'name "proc5"
                        'kind "procedure"
                        'arity 0
                        'arity-at-least true
                        'required-keywords empty
                        'all-keywords empty))

  (module p racket
    (provide (all-defined-out))

    (define bool-id true)
    (define int-id 99)
    (define str-id "hello")
    (define sym-id 'my-symbol)
    (define float-id 12.3)
    (struct point (x y) #:transparent)
    ;; foo has three positional arguments, 2 keyword arguments
    (define (foo a b [c 3] #:kw1 kw1 #:kw2 kw2)
      1)

    ;; bar has two positional arguments, and takes arbitrary
    ;; number of arguments afterwards
    (define (bar a b . c)
      1))
  ;; test extract-module-info extracts all exported identifiers
)
