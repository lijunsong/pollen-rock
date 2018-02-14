#lang racket

;;; API: get filesystem contents (list directory or get file contents)


(require "../http-util.rkt")
(require "../logger.rkt")
(require json)
(require web-server/http/request-structs)
(require (prefix-in pollen: pollen/private/file-utils))

(provide search-handler)


;; return spec for source-path
;;
;; when errno is 0, source and output is the relative
;; path. source-exists indicates whether the source exists on file
;; system when errno is non-zero, source and output is empty string
(define/contract (search-answer errno source output source-exists)
  (-> number? string? string? boolean? jsexpr?)
  (when (and (= errno 0) (or (string=? source "")
                             (string=? output "")))
    (raise 'internal-error
           "when errno is 0, source and output must be empty"))
  (hasheq 'source source
          'source-exists source-exists
          'output output
          'errno errno))


;; When the request is querying pollen source, this handler always
;; returns both the source and output path and checks only the source
;; existence. When querying a pollen output, this handler checks
;; whether any source that could produce the output exists. It returns
;; both source and output path if any such a source exists, errno is
;; set 1 otherwise.
(define (search-handler req url-parts)
  (log-rest-info "search-handler ~a" url-parts)
  (define file-path (apply build-path `(,(current-directory) ,@url-parts)))
  (with-handlers ([exn:fail? (lambda _ (search-answer 1 "" "" false))])
    (define-values (src out) (pollen:->source+output-paths file-path))
    (let ([source (path->string (find-relative-path (current-directory) src))]
          [output (path->string (find-relative-path (current-directory) out))])
      (define exists (file-exists? src))
      (search-answer 0 source output exists))))
