#lang rosette

(require 
    rosette/lib/destruct
    errortrace
    racket/format
    racket/syntax-srcloc
    math/base
)

(require syntax/quote)

; Constants
(define OPEN 'OPEN)
(define SUCCESS 'SUCCESS)
(define REFUND 'REFUND)

(define OWNER_ID 0)
(define CONTRACT_ID 1)
(define BENEF_ID 2)
(define USER_ID 3)

(define USER_NUM 4)

(define CLOSE_TIME 30)

(define GOAL 10000)
(define ZERO 0)
(define HALF_GOAL (/ GOAL 2))
(define OVERFLOW_MONEY (+ GOAL 1))

; Utils
(define (modify_vector vec pos mod) (
    vector-set! vec pos (mod (vector-ref vec pos))
))

(struct message (sender value) #:transparent)

; Temporal Logic
(struct previous (formula state) #:transparent)
(struct since (formula state given_state) #:transparent)

(struct once (formula state) #:transparent)
(struct always (formula state) #:transparent)
(struct still (formula state) #:transparent)

(define (eval_expr temporal_expr) (
  destruct temporal_expr
    [(previous formula state) (
      if (null? (get-field last_state state))
      #f
      (eval_expr (formula (get-field last_state state)))
    )]

    ; Not check whether given_state is non_moment
    [(since formula state given_state) (
      or 
      (eval_expr (formula state)) 
      (if (null? (get-field last_state state))
        #f
        (since formula (get-field last_state state))
      )
    )]

    [(once formula state) (
      since formula state (first states)
    )]

    [(always formula state) (
      and
      (eval_expr (formula state)) 
      (if (null? (get-field last_state state))
        #t
        (always formula (get-field last_state state))
      )
    )]

    [_ temporal_expr]
))

; Conditions
(define (only_owner state) (equal? (message-sender (get-field cur_message state)) OWNER_ID))
(define (withdraw_req state) (equal? (get-field crowdsale_state state) SUCCESS))
(define (claim_refund_req state) (equal? (get-field crowdsale_state state) REFUND))
(define (invest_req state) (< (get-field raised state) GOAL))
(define (close_sale_req state) (or (> (get-field now state) CLOSE_TIME) (>= (get-field raised state) GOAL)))

; Requirements
(define (r0 state) (always 
  (lambda (state)
    (equal? 
      (send state balance) 
      (send state expected_refund)
    )
  )
  state
))

(define (r1 state) (always
  (lambda (state) 
    (or
      (equal? (get-field crowdsale_state state) SUCCESS) 
      (>= (send state balance) (send state sum_deposits))
    )
  )
  state
))

(define (r2 state) (always
  (lambda (state)
    (nand
      (once (lambda (state) (get-field refund_called state)) state)
      (once (lambda (state) (get-field withdraw_called state)) state)
    )
  )
  state
))

(define (r3 state) (always
  (lambda (state)
    (nand
      (once (lambda (state) 
        (previous (lambda (state) 
          (>= (send state sum_deposits) GOAL)
        ) state)
      ) state)
      (get-field refund_called state)
    )
  )
  state
))

(define reqs (list r0 r1 r2 r3))

(define (check_req state) (map (lambda (req) (eval_expr (req state))) reqs))

; State
(define state%
  (class* object% (printable<%>)
    (super-new)
    (init-field
      [now 0]
      [crowdsale_state OPEN]
      [raised 0]

      [refund_called #f]
      [withdraw_called #f]
      [external_call_failed #f]

      [deposits (make-vector USER_NUM 0)]
      [accounts (vector 0 0 0 OVERFLOW_MONEY)]
      [cur_message (message OWNER_ID 0)]

      [last_refund_addr null]
      [last_state null]
    )

    (define/public (balance) (vector-ref accounts CONTRACT_ID))

    (define/public (expected_refund) (
      -
      (if (null? last_state)
        0
        (send last_state balance)
      )
      (if (or (null? last_refund_addr) (null? last_state))
        0
        (vector-ref (get-field deposits last_state) last_refund_addr)
      )
    ))

    (define/public (sum_deposits) (
      sum (vector->list deposits)
    ))

    (define/public (transfer_value from to value) (
        begin
        (modify_vector accounts from (lambda (x) (- x value)))
        (modify_vector accounts to (lambda (x) (+ x value)))
    ))

    ; Escrow
    (define/public (set_crowdsale_state value) (
      set! crowdsale_state value
    ))

    (define/public (deposit address) (
      modify_vector deposits address (lambda (x) (+ x (message-value cur_message)))
    ))

    (define/public (refund_deposit address) (
      let ([cur (vector-ref deposits address)])
        (begin
          (set! last_refund_addr address)
          (set! refund_called #t)
          (vector-set! deposits address 0)
          cur
        )
    ))

    (define/public (withdraw) (
      begin
      (set! withdraw_called #t)
      (transfer_value CONTRACT_ID BENEF_ID (balance))
    ))

    ; CrowdSale
    (define/public (invest) (
      begin
      (transfer_value (message-sender cur_message) CONTRACT_ID (message-value cur_message))
      (deposit (message-sender cur_message))
      (set! raised (+ raised (message-value cur_message)))
    ))

    (define/public (deepcopy) (
      new state% 
        [now now] 
        [crowdsale_state crowdsale_state]
        [raised raised]

        [refund_called refund_called]
        [withdraw_called withdraw_called]
        [external_call_failed external_call_failed]

        [deposits deposits]
        [accounts accounts]
        [cur_message cur_message]

        [last_refund_addr last_refund_addr]
        [last_state this]
    ))

    (define/public (non_moment state) (
      if (equal? this state)
        #t
        (if (null? last_state)
          #f
          (send last_state non_moment state)
        )
    ))

    (define/public (custom-print port quoting-depth) (
      print (~a 
        "Now " now ";"
        "Status " crowdsale_state ";"
        "Raised " raised ";"

        "RefundCalled " refund_called ";"
        "WithdrawCalled " withdraw_called ";"
        ; "ExtCallFailed " external_call_failed ";"

        "Deposits " deposits ";"
        "Accounts " accounts ";"
        "Message " cur_message ";"

        "LastRefundAddr " last_refund_addr ";"
        "HasLastState " (null? last_state) ";"
      )
      port
    ))

    (define/public (custom-write port) (
      write (~a "Write " "\n") port
    ))

    (define/public (custom-display port) (
      display (~a "Display " "\n") port)))
)

(define cur_state (new state%))
(define (get_cur_message) (get-field cur_message cur_state))
(define states (list cur_state))
(define (add_state) (set! states (append states (list (
    let ([cur (send cur_state deepcopy)]) (
      begin
      (set-field! last_state cur (last states))
      cur
    )
)))))

(struct set_close () #:transparent)
(struct set_refund () #:transparent)
(struct deposit (address) #:transparent)
(struct withdraw () #:transparent)
(struct claim_refund (address) #:transparent)
(struct refund (address value) #:transparent)

(struct invest () #:transparent)
(struct close_sale () #:transparent)

(struct internal_call (callable message) #:transparent)
(struct external_call (callable message) #:transparent)

(define (interpret p)
    (destruct p
      ; Escrow
      [(set_close) (
          when (only_owner cur_state) (send cur_state set_crowdsale_state SUCCESS)
      )]

      [(set_refund) (
          when (only_owner cur_state) (send cur_state set_crowdsale_state REFUND)
      )]

      [(deposit address) (
          send cur_state deposit address
      )]

      [(withdraw) (
          when (withdraw_req cur_state) (send cur_state withdraw)
      )]

      [(claim_refund address) (
        when (claim_refund_req cur_state) (
          let ([amount (send cur_state refund_deposit address)])
          (interpret (external_call (send cur_state transfer_value CONTRACT_ID address amount) (get_cur_message)))
        )
      )]

      ; CrowdSale
      [(invest) (
          when (invest_req cur_state) (send cur_state invest)
      )]

      [(close_sale) (
          when (close_sale_req cur_state) (
            if (>= (get-field raised cur_state) GOAL)
              (interpret (internal_call (set_close) (get_cur_message)))
              (interpret (internal_call (set_refund) (get_cur_message)))
          )
      )]

      ; Utils
      [(internal_call callable message) (
        begin
        (set-field! cur_message cur_state message)
        (interpret callable)
        (add_state)
      )]

      [(external_call callable message) (
        begin
        (set-field! cur_message cur_state message)
        (if (get-field external_call_failed cur_state)
            (set-field! external_call_failed cur_state #f)
            (interpret callable)
        )
      )]

      [_ p]
))

; BugTrace 1
(set-field! now cur_state (+ CLOSE_TIME 1))
(interpret (internal_call (invest) (message USER_ID HALF_GOAL)))
(interpret (internal_call (close_sale) (message OWNER_ID 0)))
(interpret (internal_call (claim_refund USER_ID) (message USER_ID 0)))
(check_req cur_state)
