#lang racket/base

#|review: ignore|#

(require review/ext
         syntax/parse/pre)

(provide
 should-review-syntax?
 review-syntax)

(define (should-review-syntax? stx)
  (syntax-case stx (define-actor)
    [(define-actor . _rest) #t]
    [_ #f]))

(define (track-args stx)
  (for ([arg-id-stx (in-list (syntax-e stx))])
    (track-binding #:check-usages? #t arg-id-stx)))

(define-syntax-class private-definition
  #:datum-literals (define/private)
  (pattern
   (define/private id:id ~! _expr:expression)
   #:do [(track-binding #'id)])
  (pattern
   (define/private ~!
     (id:id
      {~do (track-binding #'id)
           (push-scope)}
      arg-id:id ...
      {~do (track-args #'(arg-id ...))})
     {~do (push-scope)}
     _body:expression ...+
     {~do (pop-scope)
          (pop-scope)})))

(define-syntax-class method-definition
  #:datum-literals (define)
  (pattern
   (define ~!
     (id:id {~do (push-scope)} state-arg-id:id arg-id:id ...)
     {~do (track-binding #'id #:check-usages? #f)
          (track-args #'(state-arg-id arg-id ...))
          (push-scope)}
     body:expression ...+
     {~do (pop-scope)
          (pop-scope)})))

(define-syntax-class actor-definition
  #:datum-literals (define-actor :)
  (pattern (define-actor
             ~!
             {~do (push-scope)}
             (actor-id:id arg-id:id ...)
             {~do (track-args #'(arg-id ...))}
             {~alt {~optional {~seq #:state _state-expr:expression}}
                   {~optional {~seq #:event _event-proc:expression}}
                   {~optional {~seq #:receive? _receive-proc:expression}}
                   {~optional {~seq #:stopped? _stopped-proc:expression}}
                   {~optional {~seq #:on-stop _on-stop-proc:expression}}} ...
             {~alt _private:private-definition method:method-definition} ...
             {~do (pop-scope)})
           #:do [(track-binding #'actor-id #:check-usages? #t)
                 (track-binding
                  #'actor-id "~a?"
                  #:check-usages? #f
                  #:related-to #'actor-id)
                 (for* ([method-id-stx (in-list (syntax-e #'(method.id ...)))]
                        [method-suffix (in-list '("" "-evt"))])
                   (track-binding
                    method-id-stx
                    #:related-to #'actor-id
                    #:check-usages? (equal? method-suffix "")
                    (format "~~a~a" method-suffix)))]))

(define (review-syntax stx)
  (syntax-parse stx
    [d:actor-definition #'d]))
