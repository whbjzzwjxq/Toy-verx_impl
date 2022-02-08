#lang rosette/safe

(require
  "./temporal.rkt"
  "./state.rkt"
)

(provide (all-defined-out))

(define (r0 state) (always 
  (lambda (state)
    (or 
      (not (state-last_refund_called state))
      (equal?
        (get-balance state)
        (get-expected-refund state)
      )
    )
  )
  state
))

(define (r1 state) (always
  (lambda (state)
    (or
      (equal? (state-crowdsale_state state) SUCCESS) 
      (>= (get-balance state) (sum-deposits state))
    )
  )
  state
))

(define (r2 state) (always
  (lambda (state)
    (t_nand
      (once (lambda (state) (state-refund_called state)) state)
      (once (lambda (state) (state-withdraw_called state)) state)
    )
  )
  state
))

(define (r3 state) (always
  (lambda (state)
    (t_nand
      (once (lambda (state) 
        (previous (lambda (state) 
          (>= (sum-deposits state) GOAL)
        ) state)
      ) state)
      (state-refund_called state)
    )
  )
  state
))

(define reqs (list r0 r1 r2 r3))
; (define reqs (list r0))

(define (check-req state) (map (lambda (req) (eval-temp-expr (req state))) reqs))
