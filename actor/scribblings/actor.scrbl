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

@defform[
  #:literals (define)
  (define-actor (id arg ...)
    maybe-option ...
    method-definition ...)
  #:grammar ([maybe-option (code:line)
                           (code:line #:state state-expr)
                           (code:line #:event event-proc-expr)]
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

  The @racket[#:state] argument is an expression that generates the
  initial state of the actor. If not provided, the initial state of an
  actor is @racket[#f].

  @examples[
    (require actor)
    (define-actor (counter start)
      #:state start
      (define (incr state)
        (values (add1 state) state)))
    (define c (counter 5))
    (sync (incr-evt c))
    (incr c)
  ]

  The @racket[#:event] argument is a procedure that takes the current
  state and produces an additional event that the actor should handle
  (which could be a @racket[choice-evt]). If selected for
  synchronization, the synchronization result of the event will be
  used as the next state of the actor.

  @examples[
    (require actor)
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
}

@bibliography[
  @bib-entry[
    #:key "Flatt04"
    #:title "Kill-Safe Synchronization Abstractions"
    #:url "https://www.cs.tufts.edu/~nr/cs257/archive/matthew-flatt/kill-safe.pdf"]]
