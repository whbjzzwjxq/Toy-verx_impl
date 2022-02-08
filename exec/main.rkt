#lang rosette

(require
  "./state.rkt"
  "./interpret.rkt"
  "./property.rkt"
)

(define (exec calls) (
  begin
  (init-state)
  (for ([c calls]) (
    interpret (internal_call ((car c)) (cdr c))
  ))
  ; (print-states cur_state)
  (check-req cur_state)
))


; Normal Trace

(define (normal) (exec 
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

(define (solver) (solve
    (begin
      ; (assert (equal? '(#t #t #t #t) normal))
      (assert (equal? '(#t #t #t #t) (normal)))
    )
))

(solver)
; (pretty-print (normal))
