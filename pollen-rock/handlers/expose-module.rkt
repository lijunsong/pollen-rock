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
  (define value-and-type
    (cond [(boolean? val) `(,val "boolean")]
          [(number? val)  `(,val "number")]
          [(string? val)  `(,val "string")]
          [(char? val)    `(,(make-string 1 val) "char")]
          [(and (symbol? val) (not (eq? val (json-null))))
           (list (symbol->string val) "symbol")]
          [else
           (list (json-null) (json-null))]))
  (hasheq 'name name
          'kind "variable"
          'value (first value-and-type)
          'type  (second value-and-type)))


(define/contract (extract-module-bindings modules)
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
                            (var->json name v)))
       (map symbol->string ids)
       vals))







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
    (define num-id 99.9)
    (define str-id "hello")
    (define char-id #\x)
    (define sym-id 'my-symbol)
    (struct point (x y) #:transparent)
    (define mypoint (point 1 2))
    (define (foo a b [c 3] #:kw1 kw1 #:kw2 [kw2 1])
      (+ a b c))
    (define (bar a b . c)
      1)
    (define (zoo a #:kw1 kw1)
      2))

  (define/contract (search-name name json-list)
    (-> string? (listof jsexpr?) jsexpr?)
    (define elms
      (filter (lambda (json)
                (equal? (hash-ref json 'name ) name)) json-list))
    (if (empty? elms)
        (json-null)
        (first elms)))

  ;; test extract-module-bindings extracts all exported identifiers
  (define all-bindings
    (extract-module-bindings (list (quote-module-path p))))

  (let [(bool-id (search-name "bool-id" all-bindings))]
    (check-equal? bool-id (hasheq 'name "bool-id"
                                  'kind "variable"
                                  'value true
                                  'type "boolean")))

  (let [(num-id (search-name "num-id" all-bindings))]
    (check-equal? num-id (hasheq 'name "num-id"
                                 'kind "variable"
                                 'value 99.9
                                 'type "number")))

  (let [(sym-id (search-name "sym-id" all-bindings))]
    (check-equal? sym-id (hasheq 'name "sym-id"
                                 'kind "variable"
                                 'value "my-symbol"
                                 'type "symbol")))

  (let [(sym-id (search-name "char-id" all-bindings))]
    (check-equal? sym-id (hasheq 'name "char-id"
                                 'kind "variable"
                                 'value "x"
                                 'type "char")))

  (let [(mypoint (search-name "mypoint" all-bindings))]
    (check-equal? mypoint (hasheq 'name "mypoint"
                                  'kind "variable"
                                  'value (json-null)
                                  'type (json-null))))

  (let [(point (search-name "point" all-bindings))]
    (check-equal? point (hasheq 'name "point"
                                'kind "variable"
                                'value (json-null)
                                'type (json-null))))

  (let [(bar (search-name "bar" all-bindings))]
    (check-equal? bar (hasheq 'name "bar"
                              'kind "procedure"
                              'arity 2
                              'arity-at-least true
                              'required-keywords empty
                              'all-keywords empty)))

  #|
  ;; racket module thinks foo is a syntax object. Looks like
  ;; a racket bug.
  (let [(foo (search-name "foo" all-bindings))]
    (check-equal? foo (hasheq 'name "foo"
                              'kind "procedure"
                              'arity 2
                              'arity-at-least true
                              'required-keywords (list "kw1")
                              'all-keywords (list "kw1" "kw2"))))

  (let [(zoo (search-name "zoo" all-bindings))]
    (check-equal? zoo (hasheq 'name "zoo"
                              'kind "procedure"
                              'arity 1
                              'arity-at-least false
                              'required-keywords '("kw1")
                              'all-keywords '("kw1" "kw2"))))
  |#

)
