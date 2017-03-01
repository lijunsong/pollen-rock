#lang info

(define collection "pollen-rock")

(define deps '("base"
               "rackunit-lib"
               "web-server-lib"
               "pollen"))

(define build-deps '("scribble-lib" "racket-doc"))
;(define scribblings '(("scribblings/pollen-rock.scrbl" ())))
(define pkg-desc "pollen-rock is an in-browser editor (and a server) for Pollen publishing system.")
(define version "0.1")
(define pkg-authors '(junsongli))

(define raco-commands
  (list '("pollen-rock" (submod pollen-rock/pollen-rock/main raco) "launch pollen server" #f)))
