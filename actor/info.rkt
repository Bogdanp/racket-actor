#lang info

(define license 'BSD-3-Clause)
(define collection "actor")
(define deps
  '("base"
    "actor-lib"))
(define build-deps
  '("racket-doc"
    "scribble-lib"))
(define implies
  '("actor-lib"))
(define scribblings
  '(("scribblings/actor.scrbl")))
