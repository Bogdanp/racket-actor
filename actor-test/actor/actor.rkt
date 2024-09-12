#lang racket/base

(require actor
         rackunit)

(define actor-suite
  (test-suite
   "actor"

   (test-case "counter"
     (define-actor (counter start)
       #:state start
       (define (incr state)
         (values (add1 state) state)))

     (define c (counter 5))
     (check-equal? (incr c) 5)
     (check-equal? (incr c) 6)
     (check-equal? (incr c) 7))

   (test-case "cache"
     (let ([replace-sema (make-semaphore)])
       (define make-token gensym)
       (define-actor (token-cache)
         #:state (make-token)
         #:event (lambda (_state)
                   (handle-evt
                    replace-sema
                    (lambda (_)
                      (make-token))))
         (define (get-token state)
           (values state state)))
       (define cache (token-cache))
       (define t1 (get-token cache))
       (define t2 (get-token cache))
       (check-eq? t1 t2)
       (semaphore-post replace-sema)
       (sync (system-idle-evt))
       (define t3 (get-token cache))
       (check-not-eq? t1 t3)))

   (test-case "error"
     (define-actor (bad)
       (define (get-foo _state)
         (error 'get-foo "failed")))
     (define b (bad))
     (check-exn
      #rx"get-foo: failed"
      (lambda ()
        (get-foo b))))))

(module+ test
  (require rackunit/text-ui)
  (run-tests actor-suite))
