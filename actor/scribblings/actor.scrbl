#lang scribble/manual

@(require scribble/example
          (for-label actor
                     racket/base
                     racket/contract/base))

@title{Kill-Safe Actors}
@author[(author+email "Bogdan Popa" "bogdan@defn.io")]
@defmodule[actor]

This package provides a macro and runtime support for writing kill-safe
actors using the techniques described in the ``Kill-Safe Synchronization
Abstractions''@cite{Flatt04} paper.

@(define ev (make-base-eval '(begin (require actor))))

@defform[
  #:literals (define)
  (define-actor (id arg ...)
    maybe-option ...
    method-definition ...)
  #:grammar ([maybe-option (code:line)
                           (code:line #:state state-expr)
                           (code:line #:event event-proc-expr)
                           (code:line #:receive? receive?-proc-expr)
                           (code:line #:stopped? stopped?-proc-expr)
                           (code:line #:on-stop on-stop-proc-expr)]
             [method-definition (define (method-id state-arg-id arg-id ...)
                                  method-body ...+)])
]{

  Defines a procedure named @racket[id] that returns an instance of an
  actor when applied. For each @racket[method-definition], a procedure
  named @racket[method-id-evt] is defined that sends the actor a message
  to be handled by the body of that method and returns a synchronizable
  event representing the result of executing the body. Additionally, a
  @racket[method-id] procedure is defined that composes @racket[sync]
  with its associated @racket[method-id-evt] procedure, for convenience.

  Each method takes as a first argument the current state, followed by
  any arguments sent by the sender, and must return two values: the next
  state and a value to return to the sender.

  Each actor runs in its own @racket[thread/suspend-to-kill] and sending
  an actor a message will resume its thread if it has been killed. Any
  one actor is guaranteed to only be processing one message at a time.

  The @racket[#:state] argument accepts an expression that produces the
  initial state of the actor. If not provided, the initial state of an
  actor is @racket[#f].

  @examples[
    #:eval ev
    (define-actor (counter start)
      #:state start
      (define (incr state)
        (values (add1 state) state)))
    (define c (counter 5))
    (sync (incr-evt c))
    (incr c)
  ]

  The @racket[#:event] argument accepts a procedure that takes the
  current state and produces an additional event that the actor should
  handle (which could be a @racket[choice-evt]). If selected for
  synchronization, the synchronization result of the event will be used
  as the next state of the actor.

  @examples[
    #:eval ev
    (define (make-token) (gensym))
    (define (make-deadline) (+ (current-inexact-milliseconds) 1000))
    (struct state (token deadline))
    (define-actor (token-cache)
      #:state (state
               (make-token)
               (make-deadline))
      #:event (lambda (st)
                (handle-evt
                 (alarm-evt
                  (state-deadline st))
                 (lambda (_)
                   (state
                    (make-token)
                    (make-deadline)))))
      (define (get-token state)
        (values state (state-token state))))
    (define c (token-cache))
    (get-token c)
    (get-token c)
    (sleep 1)
    (get-token c)
  ]

  The @racket[#:receive?] argument accepts a procedure that takes
  a state and returns a boolean value representing whether or not
  the actor should receive new messages. The default value of
  @racket[receive?-proc-expr] is a procedure that ignores its argument
  and always returns @racket[#t].

  @examples[
    #:eval ev
    (define end-work-sema (make-semaphore))
    (define-actor (backpressure limit)
      #:state 0
      #:event (lambda (in-progress)
                (handle-evt
                 end-work-sema
                 (lambda (_)
                   (sub1 in-progress))))
      #:receive? (lambda (in-progress)
                   (< in-progress limit))
      (define (start-work in-progress)
        (values (add1 in-progress) #t)))
    (define bp (backpressure 2))
    (start-work bp)
    (start-work bp)
    (sync/timeout 0.5 (start-work-evt bp))
    (semaphore-post end-work-sema)
    (sync/timeout 0.5 (start-work-evt bp))
  ]

  The @racket[#:stopped?] argument accepts a procedure that takes a
  state and returns a boolean value representing whether or not the
  actor should stop running its internal event loop. When this procedure
  returns @racket[#t], the actor stops receiving new messages and
  drains any pending responses to senders before finally applying the
  @racket[on-stop-proc-expr] with the final state.

  @examples[
    #:eval ev
    (define-actor (stoppable)
      #:state #f
      #:on-stop (λ (_) (eprintf "actor stopped!~n"))
      #:stopped? values
      (define (stop _)
        (values #t #t)))
    (define s (stoppable))
    (stop s)
  ]

  The @racket[#:on-stop] argument accepts a procedure that is called
  with the final state when the actor stops running its event loop. The
  default value of the @racket[on-stop-proc-expr] is @racket[void].
}

@section{Reference}

@defproc[(actor? [v any/c]) boolean?]{
  Returns @racket[#t] when @racket[v] is an actor.
}

@defproc[(actor-dead-evt [a actor?]) evt?]{
  Returns an event that is ready for synchronization iff @racket[a] has
  terminated. The synchronization result of an actor-dead event is the
  actor-dead event itself.
}

@bibliography[
  @bib-entry[
    #:key "Flatt04"
    #:title "Kill-Safe Synchronization Abstractions"
    #:url "https://www.cs.tufts.edu/~nr/cs257/archive/matthew-flatt/kill-safe.pdf"]]
