#lang rosette/safe

(require 
  rosette/lib/destruct
  "./state.rkt"
  "./state_modifier.rkt"
)

(provide (all-defined-out))

; Conditions
(define (only_owner state) (equal? (get-msg-sender state) OWNER_ID))
(define (withdraw_req state) (equal? (state-crowdsale_state state) SUCCESS))
(define (claim_refund_req state) (equal? (state-crowdsale_state state) REFUND))
(define (close_sale_req state) (or (> (state-now state) CLOSE_TIME) (>= (state-raised state) GOAL)))
(define (invest_req state) (< (state-raised state) GOAL))
(define (fallback_sketch state address amount) (
  pretty-print "DoNothing"
))
(define now 0)


(struct set_close () #:transparent)
(struct set_refund () #:transparent)
(struct deposit () #:transparent)
(struct withdraw () #:transparent)
(struct claim_refund () #:transparent)
(struct refund (address value) #:transparent)

(struct invest () #:transparent)
(struct close_sale () #:transparent)

(struct internal_call (callable message) #:transparent)
(struct external_call (callable message) #:transparent)

(struct set_overflow_time () #:transparent)
(struct set_external_call_failed () #:transparent)

(define cur_state (init-state))

(define (interpret p)
    (destruct p
      ; Escrow
      [(set_close) (
        when (only_owner cur_state) (set-state-crowdsale_state! cur_state SUCCESS)
      )]

      [(set_refund) (
        when (only_owner cur_state) (set-state-crowdsale_state! cur_state REFUND)
      )]

      [(deposit) (
        let* (
          [msg-sender (get-msg-sender cur_state)]
          [msg-value (get-msg-value cur_state)]
        ) (
          begin
          (transfer-value cur_state msg-sender CONTRACT_ID msg-value)
          (add-deposit cur_state msg-sender msg-value)
        )
      )]

      [(withdraw) (
        when (withdraw_req cur_state) (
          begin
          (set-state-withdraw_called! cur_state #t)
          (transfer-value cur_state CONTRACT_ID BENEF_ID (get-balance cur_state))
        )
      )]

      [(claim_refund) (
        when (claim_refund_req cur_state) (
          begin
          (set-state-last_refund_addr! cur_state (get-msg-sender cur_state))
          (set-state-refund_called! cur_state #t)
          (set-state-last_refund_called! cur_state #t)
          (let* 
            (
              [address (get-msg-sender cur_state)]
              [amount (get-deposit cur_state address)]
            ) 
            (
              begin
              (vector-set! (state-deposits cur_state) address 0)
                (let
                  ([res (interpret
                    (external_call (refund address amount) (state-cur_message cur_state)))
                  ])
                  (unless res (fallback_sketch cur_state address amount))
                )
            )
          )
        )
      )]

      ; CrowdSale
      [(invest) (
          when (invest_req cur_state) (
            let* (
              [msg-sender (get-msg-sender cur_state)]
              [msg-value (get-msg-value cur_state)]
            ) 
            (
              begin
              (interpret (internal_call (deposit) (state-cur_message cur_state)))
              (add-raised cur_state msg-value)
            )
          )
      )]

      [(close_sale) (
          when (close_sale_req cur_state) (
            if (>= (state-raised cur_state) GOAL)
              (interpret (internal_call (set_close) (state-cur_message cur_state)))
              (interpret (internal_call (set_refund) (state-cur_message cur_state)))
          )
      )]

      ; Calls
      [(internal_call callable message) (
        begin
        (set! cur_state (deepcopy cur_state))
        (set-state-cur_message! cur_state message)
        (set-state-last_refund_called! cur_state #f)
        (interpret callable)
      )]

      [(external_call callable message) (
        begin
        (set-state-cur_message! cur_state message)
        (let ([failed (state-external_call_failed cur_state)]) (
          begin
          (if (state-external_call_failed cur_state)
            (set-state-external_call_failed! cur_state #f)
            (interpret callable)
          )
          (not failed)
        ))
      )]

      ; Utils
      [(set_overflow_time) (
        set-state-now! cur_state (+ CLOSE_TIME 1)
      )]

      [(set_external_call_failed) (
        set-state-external_call_failed! cur_state #t
      )]

      [(refund address value) (
        begin
        ; (pretty-print 'Coming-refund)
        ; (pretty-print (cons address value))
        (transfer-value cur_state CONTRACT_ID address value)
      )]

      [_ p]
))
