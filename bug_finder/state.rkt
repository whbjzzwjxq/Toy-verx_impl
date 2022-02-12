#lang rosette

(require 
  racket/struct
)

(provide (all-defined-out))

; Constants
(define OPEN 0)
(define SUCCESS 1)
(define REFUND 2)

(define OWNER_ID 0)
(define CONTRACT_ID 1)
(define BENEF_ID 2)
(define USER_ID 3)

(define USER_NUM 4)

(define CLOSE_TIME 30)

(define GOAL 10000)
(define ZERO 0)
(define HALF_GOAL (/ GOAL 2))
(define OVERFLOW_GOAL (+ GOAL 1))
(define ALL_VALUE (* OVERFLOW_GOAL 2))
(define -GOAL (- GOAL))

(struct message (sender value) #:transparent)
(define (default-message) (message OWNER_ID 0))

(define (default-vec) (vector 0 0 0 0))


(struct state 
  (
    [now #:mutable]
    [crowdsale_state #:mutable]
    [raised #:mutable]
    [last_refund_called #:mutable]
    [refund_called #:mutable]
    [withdraw_called #:mutable]
    [external_call_failed #:mutable]
    [deposits #:mutable]
    [accounts #:mutable]
    [cur_message #:mutable]
    [last_refund_addr #:mutable]
    [last_state #:mutable]
  )
  #:transparent
  #:methods gen:custom-write
  [(define write-proc
    (make-constructor-style-printer
      (lambda (obj) 'State)
      (lambda (obj) 
        (list
          (cons 'Status (state-crowdsale_state obj))
          (cons 'Raised (state-raised obj))
          (cons 'RefundCalled (state-refund_called obj))
          (cons 'WithdrawCalled (state-withdraw_called obj))
          (cons 'ExtCallFailed (state-external_call_failed obj))
          (cons 'Deposits (state-deposits obj))
          (cons 'Accounts (state-accounts obj))
          (cons 'Message (state-cur_message obj))
          (cons 'LastRefundCalled (state-last_refund_called obj))
          (cons 'LastRefundAddr (state-last_refund_addr obj))
          (cons 'IsRoot (null? (state-last_state obj)))
        )
      )
    )
  )]
)

(define (get-equal-items state) (
  let ([vec (struct->vector state)]) (
    vector->values vec 0 (- (vector-length vec) 1)
  )
))

(define (state-eq? state-a state-b) (equal? (get-equal-items state-a) (get-equal-items state-b)))

(define (modify-vector vec pos mod) (
  vector-set! vec pos (mod (vector-ref vec pos))
))

(define (init-state) (
  state
    0
    OPEN
    0
    #f
    #f
    #f
    #f
    (default-vec)
    (vector 0 0 0 ALL_VALUE)
    (default-message)
    null
    null
  )
)

(define (deepcopy-vec vec) (
  let ([new-vec (default-vec)]) (
    begin
    (vector-copy! new-vec 0 vec)
    new-vec
  )
))

(define (deepcopy _state) (
  state
  (state-now _state)
  (state-crowdsale_state _state)
  (state-raised _state)
  (state-last_refund_called _state)
  (state-refund_called _state)
  (state-withdraw_called _state)
  (state-external_call_failed _state)
  (deepcopy-vec (state-deposits _state))
  (deepcopy-vec (state-accounts _state))
  (state-cur_message _state)
  (state-last_refund_addr _state)
  _state
))

(define (get-last state) (
  state-last_state state
))

(define (get-msg-sender state) (
  message-sender (state-cur_message state)
))

(define (get-msg-value state) (
  message-value (state-cur_message state)
))

(define (print-states state) (
  begin
  (unless (null? (get-last state))
    (print-states (get-last state))
  )
  (pretty-print state)
))
