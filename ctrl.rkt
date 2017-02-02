#lang racket

(require "config.rkt")
(require "model.rkt")
(require "util.rkt")

(provide (all-defined-out))

;;
(define/contract (ctrl/all-files resource)
  (-> resource? (listof file?))
  (data/all-files resource))

(define (ctrl/file-name f)
  (file-name f))

(define (ctrl/file-resource f)
  (file-resource f))

(define (ctrl/file-is-directory? f)
  (file-directory? f))

(define (ctrl/file-ptype f)
  (file-ptype f))

;; test whether the file has pollen suffix
(define (ctrl/has-pollen-suffix? fname)
  (if (data/pollentype fname) #t #f))
