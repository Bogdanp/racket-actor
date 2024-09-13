#lang info

(define license 'BSD-3-Clause)
(define collection "actor")
(define deps
  '("base"
    "review"))
(define review-exts
  '([actor/review should-review-syntax? review-syntax]))
