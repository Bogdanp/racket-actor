#lang racket/base

(require (for-syntax racket/base
                     racket/syntax
                     syntax/parse/pre)
         data/monocle
         racket/match)

(provide
 define-actor)

(begin-for-syntax
  (define-syntax-class method-definition
    #:literals (define)
    (pattern
     (define (id:id state-arg-id:id arg-id:id ...)
       body ...+))))

(define-syntax (define-actor stx)
  (syntax-parse stx
    [(_ (actor-id:id arg-id:id ...)
        {~alt {~optional {~seq #:state state-expr}}
              {~optional {~seq #:event event-proc}}} ...
        method:method-definition ...)
     #:with st (format-id stx "st")
     #:with (method-evt-id ...)
     (for/list ([method-id-stx (in-list (syntax-e #'(method.id ...)))])
       (format-id method-id-stx "~a-evt" method-id-stx))
     #'(begin
         (define (actor-id arg-id ...)
           (make-actor
            'actor-id
            (lambda () {~? state-expr #f})
            {~? event-proc (lambda (_) never-evt)}
            (lambda (st id args)
              (case id
                [(method.id)
                 (apply
                  (lambda (method.state-arg-id method.arg-id ...)
                    method.body ...)
                  st args)] ...))))
         (define (method-evt-id a method.arg-id ...)
           (actor-evt a 'method.id method.arg-id ...)) ...
         (define (method.id a method.arg-id ...)
           (sync (method-evt-id a method.arg-id ...))) ...)]))

(struct actor (ch thd))
(struct req (res res-ch nack-evt))

(define-logger actor)
(struct actor-state (reqs state))
(define-struct-lenses actor-state)

(define (make-actor who make-state make-event method-proc)
  (define ch (make-channel))
  (define thd
    (thread/suspend-to-kill
     (lambda ()
       (let loop ([st (actor-state null (make-state))])
         (loop
          (with-handlers ([exn:fail?
                           (lambda (e)
                             (begin0 st
                               ((error-display-handler)
                                (format "~a: ~a" who (exn-message e))
                                e)))])
            (apply
             sync
             (handle-evt
              ch
              (match-lambda
                [`(,id ,res-ch ,nack-evt . ,args)
                 (define-values (next-st res)
                   (with-handlers ([exn:fail?
                                    (lambda (e)
                                      (values (actor-state-state st) e))])
                     (method-proc
                      (actor-state-state st)
                      id args)))
                 (&actor-state-state
                  (lens-update
                   &actor-state-reqs st
                   (λ (reqs) (cons (req res res-ch nack-evt) reqs)))
                  next-st)]
                [message
                 (begin0 st
                   (log-actor-error "~a: invalid message ~.s" who message))]))
             (handle-evt
              (make-event (actor-state-state st))
              (lambda (next-st)
                (&actor-state-state st next-st)))
             (append
              (for/list ([r (in-list (actor-state-reqs st))])
                (handle-evt
                 (req-nack-evt r)
                 (lambda (_)
                   (lens-update &actor-state-reqs st (λ (reqs) (remq r reqs))))))
              (for/list ([r (in-list (actor-state-reqs st))])
                (handle-evt
                 (channel-put-evt
                  (req-res-ch r)
                  (req-res r))
                 (lambda (_)
                   (lens-update &actor-state-reqs st (λ (reqs) (remq r reqs))))))))))))))
  (actor ch thd))

(define (actor-evt a id . args)
  (wrap-evt
   (nack-guard-evt
    (lambda (nack-evt)
      (match-define (actor ch thd) a)
      (define res-ch (make-channel))
      (begin0 res-ch
        (thread-resume thd (current-thread))
        (channel-put ch (list* id res-ch nack-evt args)))))
   (lambda (res-or-exn)
     (begin0 res-or-exn
       (when (exn:fail? res-or-exn)
         (raise res-or-exn))))))
