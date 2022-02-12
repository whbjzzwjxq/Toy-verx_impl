#lang rosette

(require
  "./state.rkt"
  "./abstract.rkt"
)

(provide (all-defined-out))

(define add +/a)
(define sub -/a)

(define (get-expected-refund state) (
  let* (
    [last_state (state-last_state state)]
    [last_refund_addr (state-last_refund_addr state)]
  ) (
    sub
    (if (null? last_state)
      0
      (get-balance last_state)
    )
    (if (or (null? last_refund_addr) (null? last_state))
      0
      (get-deposit last_state last_refund_addr)
    )
  )
))

(define (get-balance state) (
  vector-ref (state-accounts state) CONTRACT_ID
))

(define (get-deposit state address) (
  vector-ref (state-deposits state) address
))

(define (add-deposit state address value) (
  modify-vector (state-deposits state) address (lambda (x) (add x value))
))

(define (add-account state address value) (
  modify-vector (state-accounts state) address (lambda (x) (add x value))
))

(define (add-raised state value) (
  set-state-raised! state (add (state-raised state) value)
))

(define (sum-deposits state) (
  apply add (vector->list (state-deposits state))
))

(define (sum-accounts state) (
  apply add (vector->list (state-accounts state))
))

(define (transfer-value state from to value) (
  begin
  (add-account state from (sub value))
  (add-account state to (add value))
))
