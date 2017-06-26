#lang racket

(require json)
(require web-server/http/request-structs)
(require web-server/http/response-structs)

(define RPC:REFERENCE-ERROR "ReferenceError")
(define RPC:ARITY-ERROR "ArityError")
(define RPC:NOT-RPC-ERROR "NotRPCError")


(define/contract (rpc-answer id
                             #:error [e (json-null)]
                             #:result [r (json-null)])
  (->* (bytes?)
       (#:error jsexpr? #:result jsexpr?)
       bytes?)
  (jsexpr->bytes
   (hasheq 'id (bytes->string/utf-8 id)
           'error e
           'result r)))


(define (rpc-error-answer id error)
  (rpc-answer id #:error error))

(define (rpc-result-answer id result)
  (rpc-answer id #:result result))

(define/contract (export-rpc-handler exports)
  (-> (hash/c bytes? procedure?)
      (-> request? response?))
  (define (handler req)
    (define bindings (request-bindings/raw req))
    (define/contract (extract-bindings key)
      (-> bytes? (or/c false? bytes?))
      (let [(form (bindings-assq key bindings))]
        (if form (binding:form-value form) false)))
    (define call-id (extract-bindings #"id"))
    (define method-name (extract-bindings #"method"))
    (define params-string (extract-bindings #"params"))

    (define method (hash-ref exports method-name false))
    (define answer
      (cond [(and call-id method params-string)
             (define params (bytes->jsexpr params-string))
             (with-handlers
                 ([exn:fail:contract:arity?
                   (lambda (c) (rpc-error-answer call-id RPC:ARITY-ERROR))])
               (define result (apply method params))
               (rpc-result-answer call-id result))]
            [(or (not call-id) (not params-string))
             (rpc-error-answer #"-1" RPC:NOT-RPC-ERROR)]
            [else
             (rpc-error-answer call-id RPC:REFERENCE-ERROR)]))
    (response/output
     (lambda (op) (write-bytes answer op))))

  handler)

;(module+ test
(require rackunit)
(require net/url-string)

(define (rpc-request id method params)
  (make-request
   #"POST" (string->url "http://localhost:8000") empty
   (delay
     (list (make-binding:form #"method" method)
           (make-binding:form #"params" params)
           (make-binding:form #"id" id)))
   #"fake post raw"
   "0.0.0.0" 8000
   "0.0.0.0"))

(define (rpc-bad-request id method params)
  (make-request
   #"POST" (string->url "http://localhost:8000") empty
   (delay
     (list (make-binding:form #"method" method)
           (make-binding:form #"params-typo" params)
           (make-binding:form #"id" id)))
   #"fake post raw"
   "0.0.0.0" 8000
   "0.0.0.0"))

(define (pollen-append b) (string-append "pollen-" b))

(define api-handler
  (export-rpc-handler
   (hash #"add" +
         #"pollen-append" pollen-append)))

(define (get-response-json response)
  (define output (call-with-output-string (response-output response)))
  (string->jsexpr output))

(define (check-response-field-equal? response field expected)
  (define json (get-response-json response))
  (check-equal? (hash-ref json field false) expected))

;; fact test rpc call
(let [(response (api-handler (rpc-request #"1" #"add" #"[1, 2]")))]
  (check-response-field-equal? response 'error (json-null))
  (check-response-field-equal? response 'result 3))

;; fact test rpc call
(let [(response (api-handler (rpc-request #"1" #"pollen-append" #"[\"abc\"]")))]
  (check-response-field-equal? response 'result "pollen-abc")
  (check-response-field-equal? response 'error (json-null)))

;; test rpc call of non-existing functions
(let [(response (api-handler (rpc-request #"1" #"not-exist" #"[]")))]
  (check-response-field-equal? response 'result (json-null))
  (check-response-field-equal? response 'error RPC:REFERENCE-ERROR))

;; test rpc call that has mismatched arity
(let [(response
       (api-handler (rpc-request #"1" #"pollen-append" #"[\"a\", \"b\"]")))]
  (check-response-field-equal? response 'result (json-null))
  (check-response-field-equal? response 'error RPC:ARITY-ERROR))

;; test rpc call that has bad forms
(let [(response (api-handler (rpc-bad-request #"1" #"add" #"[1, 2]")))]
  (check-response-field-equal? response 'error RPC:NOT-RPC-ERROR)
  (check-response-field-equal? response 'result (json-null)))