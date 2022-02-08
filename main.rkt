#lang rosette

(require
  "./state.rkt"
  "./interpret.rkt"
  "./property.rkt"
  rosette/lib/synthax
)

(define (exec calls) (
  begin
  (set! cur_state (init-state))
  (set-state-now! cur_state now)
  (for ([c calls]) (
    interpret (internal_call ((car c)) (cdr c))
  ))
  ; (print-states cur_state)
  cur_state
))


; Normal Trace

(define (normal1) (exec 
  (list 
    (cons invest (message USER_ID HALF_GOAL))
  )
))

(define (normal2) (exec 
  (list 
    (cons invest (message USER_ID HALF_GOAL))
    (cons set_overflow_time (default-message))
    (cons close_sale (default-message)) 
    (cons claim_refund (message USER_ID 0))
  )
))


(define (bug_trace1) (exec
  (list 
    (cons invest (message USER_ID HALF_GOAL))
    (cons set_overflow_time (default-message))
    (cons close_sale (default-message))
    (cons claim_refund (message USER_ID 0))
    (cons invest (message USER_ID OVERFLOW_GOAL))
    (cons close_sale (default-message))
    (cons withdraw (default-message))
  )
))

(define syn (synthesize
  #:forall (list now)
  #:guarantee
  (begin
    (assume (>= now 0))
    (assume (<= now CLOSE_TIME))
    (assert (equal? (get-balance (normal1)) HALF_GOAL))
    (assert (equal? '(#t #t #t #t) (check-req (normal2))))
    (assert (equal? '(#t #t #t #t) (check-req (bug_trace1))))
  )
))

(if (sat? syn)
  (
    begin
    (pretty-print 'sat)
    (print-forms syn)
  )
  (pretty-print 'unsat)
)
