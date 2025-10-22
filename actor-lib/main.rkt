#lang racket/base

(#%declare #:unsafe)

(require (for-syntax racket/base
                     racket/syntax
                     syntax/parse/lib/function-header
                     syntax/parse/pre)
         racket/match)

(provide
 define/private
 define-actor
 actor?
 actor-dead-evt)

(define-syntax (define/private stx)
  (raise-syntax-error #f "may only be used inside a define-actor form" stx))

(begin-for-syntax
  (define-syntax-class method-definition
    #:literals (define)
    (pattern
     (define (id:id state-id:id . args)
       body ...+)
     #:attr expr #'(λ (state-id . args) body ...)))

  (define-syntax-class private-definition
    #:literals (define/private)
    (pattern
     (define/private id:id expr:expr))
    (pattern
     (define/private hdr:function-header
       body ...+)
     #:attr id #'hdr.name
     #:attr expr #'(λ hdr.params body ...))))

(define-syntax (define-actor stx)
  (syntax-parse stx
    [(_ (actor-id:id arg-id:id ...)
        {~alt {~optional {~seq #:state state-expr}}
              {~optional {~seq #:event event-proc}}
              {~optional {~seq #:receive? receive-proc}}
              {~optional {~seq #:stopped? stopped-proc}}
              {~optional {~seq #:on-stop on-stop-proc}}} ...
        {~alt private:private-definition
              method:method-definition} ...)
     #:with actor-id? (format-id stx "~a?" #'actor-id)
     #:with st (format-id stx "st")
     #:with (method-evt-id ...)
     (for/list ([method-id-stx (in-list (syntax-e #'(method.id ...)))])
       (format-id method-id-stx "~a-evt" method-id-stx))
     (syntax/loc stx
       (begin
         (define (actor-id? v)
           (and (actor? v)
                (eq? actor-id (actor-ctor v))))
         (define (actor-id arg-id ...)
           (letrec ([private.id private.expr]
                    ...
                    [method.id method.expr]
                    ...)
             (make-actor
              #:state (lambda () {~? state-expr #f})
              #:event {~? event-proc (lambda (_) never-evt)}
              #:receive? {~? receive-proc (lambda (_) #t)}
              #:stopped? {~? stopped-proc (lambda (_) #f)}
              #:on-stop {~? on-stop-proc void}
              'actor-id actor-id
              (lambda (st id args)
                (case id
                  [(method.id)
                   (apply method.id st args)] ...)))))
         (define (method-evt-id a . method.args)
           (actor-evt a 'method.id . method.args)) ...
         (define (method.id a . method.args)
           (sync (method-evt-id a . method.args))) ...))]))

(struct actor (who ctor ch thd))
(struct req (res res-ch nack-evt))
(struct msg (id res-ch nack-evt args))

(struct err (e))
(struct ok (v))

(define-logger actor)
(struct actor-state (reqs state))

(define (make-actor
         #:state make-state
         #:event make-event
         #:on-stop on-stop-proc
         #:stopped? stopped?-proc
         #:receive? receive?-proc
         who ctor method-proc)
  (define ch (make-channel))
  (define thd
    (thread/suspend-to-kill
     (procedure-rename
      (lambda ()
        (let loop ([actor-st (actor-state null (make-state))])
          (match-define (actor-state reqs st) actor-st)
          (define stopped?
            (stopped?-proc st))
          (define receive?
            (and (not stopped?)
                 (receive?-proc st)))
          (if (and stopped? (null? reqs))
              (on-stop-proc st)
              (loop
               (with-handlers ([exn:fail?
                                (lambda (e)
                                  ((error-display-handler)
                                   (format "~a: ~a" who (exn-message e))
                                   e)
                                  actor-st)])
                 (apply
                  sync
                  (handle-evt
                   (if receive? ch never-evt)
                   (match-lambda
                     [(msg id res-ch nack-evt args)
                      (define-values (next-st res)
                        (with-handlers ([exn:fail?
                                         (lambda (e)
                                           (values st (err e)))])
                          (define-values (next-st res)
                            (method-proc st id args))
                          (values next-st (ok res))))
                      (define the-req
                        (req res res-ch nack-evt))
                      (struct-copy
                       actor-state actor-st
                       [state next-st]
                       [reqs (cons the-req reqs)])]
                     [message
                      (log-actor-error "~a: invalid message ~.s" who message)
                      actor-st]))
                  (handle-evt
                   (if stopped? never-evt (make-event st))
                   (lambda (next-st)
                     (struct-copy
                      actor-state actor-st
                      [state next-st])))
                  (append
                   (for/list ([r (in-list reqs)])
                     (handle-evt
                      (req-nack-evt r)
                      (lambda (_)
                        (struct-copy
                         actor-state actor-st
                         [reqs (remq r reqs)]))))
                   (for/list ([r (in-list reqs)])
                     (handle-evt
                      (channel-put-evt
                       (req-res-ch r)
                       (req-res r))
                      (lambda (_)
                        (struct-copy
                         actor-state actor-st
                         [reqs (remq r reqs)])))))))))))
      (string->symbol
       (format "actor:~a" who)))))
  (actor who ctor ch thd))

(define (actor-evt a id . args)
  (handle-evt
   (nack-guard-evt
    (lambda (nack-evt)
      (match-define (actor who _ ch thd) a)
      (define res-ch (make-channel))
      (thread-resume thd (current-thread))
      (choice-evt
       (handle-evt
        (thread-dead-evt thd)
        (lambda (_) (error who "stopped")))
       (replace-evt
        (channel-put-evt ch (msg id res-ch nack-evt args))
        (lambda (_) res-ch)))))
   (match-lambda
     [(err e) (raise e)]
     [(ok v) v])))

(define (actor-dead-evt a)
  (thread-dead-evt (actor-thd a)))
