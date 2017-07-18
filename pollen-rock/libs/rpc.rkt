#lang racket

(require json)
(require web-server/http/request-structs)
(require web-server/http/response-structs)

(provide export-rpc-handler
         rpc-raise
         exn:fail:rpc
         exn:fail:rpc:unbound
         exn:fail:rpc:arity
         exn:fail:rpc:bad-request)

;; built-in rpc exceptions. All user-defined rpc exception
;; must extends exn:fail:rpc
(struct exn:fail:rpc exn:fail ())
(struct exn:fail:rpc:unbound exn:fail:rpc ())
(struct exn:fail:rpc:arity exn:fail:rpc ())
(struct exn:fail:rpc:bad-request exn:fail:rpc ())
(struct exn:fail:rpc:server-exn exn:fail:rpc ())

;; raise rpc exceptions
(define (rpc-raise exn-type [msg ""])
  (raise
   (exn-type msg (current-continuation-marks))))

;; helper function to construct rpc's answer
(define/contract (rpc-answer id
                             #:error [e (json-null)]
                             #:result [r (json-null)])
  (->* (bytes?)
       (#:error jsexpr? #:result jsexpr?)
       bytes?)
  (jsexpr->bytes
   (hasheq 'id (bytes->string/utf-8 id) 'error e 'result r)))

(define (rpc-error-answer id error)
  (rpc-answer id #:error error))

(define (rpc-result-answer id result)
  (rpc-answer id #:result result))

(define/contract (exn->rpc-answer e [id #f])
  (-> exn:fail? bytes?)
  ;; RPC error name will be reported.
  ;; Other racket error message will be reported
  (define message (if (exn:fail:rpc? e)
                      (let* ([exn-vec (struct->vector e)]
                             [struct-name (symbol->string (vector-ref exn-vec 0))])
                        (string-trim struct-name "struct:"))
                      (exn-message e)))
  ((error-display-handler) message e)
  (rpc-error-answer (if id id #"-1") message))


;; This function returns a request handler that handles requests conforming
;; to json-rpc protocol
;;
;; exports is a hash table maps from the desired names to procedures. Procedure
;; can be any racket function, as long as it returns a jsexpr
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
    (define params-content (extract-bindings #"params"))

    (define method (hash-ref exports method-name false))
    (define answer
      (with-handlers
          ([exn:fail:rpc? (lambda (e) (exn->rpc-answer e))]
           [exn:fail? (lambda (e) (exn->rpc-answer e))])
        (cond [(and call-id method params-content)
               (define params (bytes->jsexpr params-content))
               (with-handlers
                   ([exn:fail:contract:arity? (lambda (e) (rpc-raise exn:fail:rpc:arity))])
                 (define result (apply method params))
                 (rpc-result-answer call-id result))]
              [(or (not call-id) (not params-content))
               (rpc-raise exn:fail:rpc:bad-request)]
              [else
               ;; method is not found
               (rpc-raise exn:fail:rpc:unbound)])))
    (response/output
     (lambda (op) (write-bytes answer op))))

  handler)

(module+ test
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
           #"my-append" pollen-append)))

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
  (let [(response (api-handler (rpc-request #"1" #"my-append" #"[\"abc\"]")))]
    (check-response-field-equal? response 'result "pollen-abc")
    (check-response-field-equal? response 'error (json-null)))

  ;; test rpc call of non-existing functions
  (let [(response (api-handler (rpc-request #"1" #"not-exist" #"[]")))]
    (check-response-field-equal? response 'result (json-null))
    (check-response-field-equal? response 'error "exn:fail:rpc:unbound"))

  ;; test rpc call that has mismatched arity
  (let [(response
         (api-handler (rpc-request #"1" #"my-append" #"[\"a\", \"b\"]")))]
    (check-response-field-equal? response 'result (json-null))
    (check-response-field-equal? response 'error "exn:fail:rpc:arity"))

  ;; test rpc call that has bad forms
  (let [(response (api-handler (rpc-bad-request #"1" #"add" #"[1, 2]")))]
    (check-response-field-equal? response 'error "exn:fail:rpc:bad-request")
    (check-response-field-equal? response 'result (json-null)))
)
