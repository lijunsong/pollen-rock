#lang racket

#|
This file defines functions that return Json object used in api.

This file is the source of truth of the spec of Json object.
|#


(module+ test
  (require rackunit)
  (require json))

(define (list-directory-item name type)
  (hasheq 'file-name name
          'file-type type))

(define/contract (list-directory-answer dirs files)
  (-> (listof string?) (listof string?) (listof (hash/c symbol? string?)))
  (define dir-list (map (lambda (name)
                          (list-directory-item name "d")) dirs))
  (define file-list (map (lambda (name)
                           (list-directory-item name "f")) files))
  (append dir-list file-list))

(module+ test
  (define answer (list-directory-answer '("d1") '("f1")))
  (check-equal? (list (list-directory-item "d1" "d")
                      (list-directory-item "f1" "f"))
                answer)
  )
