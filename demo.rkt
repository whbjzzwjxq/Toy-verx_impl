#lang rosette

(require 
    rosette/lib/destruct
    errortrace
    racket/format
    racket/syntax-srcloc
    math/base
)

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
(define default_message (message OWNER_ID 0))

; Temporal Logic
(struct tm_pred () #:transparent)
(struct previous tm_pred (formula state) #:transparent)
(struct since tm_pred (formula state given_state) #:transparent)

(struct once tm_pred (formula state) #:transparent)
(struct always tm_pred (formula state) #:transparent)
(struct still tm_pred (formula state) #:transparent)

(struct t_and (x y) #:transparent)
(struct t_nand (x y) #:transparent)
(struct t_or (x y) #:transparent)

(define (eval_expr _expr) (
  destruct _expr
    [(previous formula state) (
      if (null? (get_last state))
      #f
      (eval_expr (formula (get_last state)))
    )]

    ; Not check whether given_state is non_moment
    [(since formula state given_state) (
      or
      (eval_expr (formula state))
      (if (null? (get_last state))
        #f
        (if (or (null? given_state) (not (equal? (get-field last_state given_state) state)))
          (eval_expr (since formula (get_last state) given_state))
          #f
        )
      )
    )]

    [(once formula state) (
      eval_expr (since formula state null)
    )]

    [(always formula state) (
      and
      (eval_expr (formula state))
      (if (null? (get_last state))
        #t
        (eval_expr (always formula (get_last state)))
      )
    )]

    [(t_and x y) (
      and
      (eval_expr x)
      (eval_expr y)
    )]

    [(t_nand x y) (
      nand
      (eval_expr x)
      (eval_expr y)
    )]

    [(t_or x y) (
      and
      (eval_expr x)
      (eval_expr y)
    )]

    [_ _expr]
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
    (or 
      (not (get-field last_refund_called state))
      (equal?
        (send state balance) 
        (send state expected_refund)
      )
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
    (t_nand
      (once (lambda (state) (get-field refund_called state)) state)
      (once (lambda (state) (get-field withdraw_called state)) state)
    )
  )
  state
))

(define (r3 state) (always
  (lambda (state)
    (t_nand
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

      [last_refund_called #f]
      [refund_called #f]
      [withdraw_called #f]
      [external_call_failed #f]

      [deposits (make-vector USER_NUM 0)]
      [accounts (vector 0 0 0 OVERFLOW_MONEY)]
      [cur_message default_message]

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
      let ([new_state (new state%)]) (
        begin
        (for ([name (field-names this)]) (
          dynamic-set-field! name new_state (dynamic-get-field name this)
        ))
        (set-field! last_state new_state this)
        (set-field! deposits new_state (vector-copy deposits))
        (set-field! accounts new_state (vector-copy accounts))
        new_state
    )))

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
        "ExtCallFailed " external_call_failed ";"

        "Deposits " deposits ";"
        "Accounts " accounts ";"
        "Message " cur_message ";"

        ; "LastRefundCalled " last_refund_called ";"
        "LastRefundAddr " last_refund_addr ";"
        "IsRoot " (null? last_state) ";"
      )
      port
    ))

    (define/public (custom-write port) (
      write (~a "Write " "\n") port
    ))

    (define/public (custom-display port) (
      display (~a "Display " "\n") port)))
)

(define cur_state null)
(define (init_state) (set! cur_state (new state%)))
(define (get_cur_message) (get-field cur_message cur_state))
(define (print_states state) (
  begin
  (pretty-print state)
  (unless (null? (get_last state))
    (print_states (get_last state))
  )
))
(define (get_last state) (get-field last_state state))

(struct set_close () #:transparent)
(struct set_refund () #:transparent)
(struct withdraw () #:transparent)
(struct claim_refund () #:transparent)
(struct refund (address value) #:transparent)

(struct invest () #:transparent)
(struct close_sale () #:transparent)

(struct internal_call (callable message) #:transparent)
(struct external_call (callable message) #:transparent)

(struct set_overflow_time () #:transparent)
(struct set_external_call_failed () #:transparent)


(define (interpret p)
    (destruct p
      ; Escrow
      [(set_close) (
          when (only_owner cur_state) (send cur_state set_crowdsale_state SUCCESS)
      )]

      [(set_refund) (
          when (only_owner cur_state) (send cur_state set_crowdsale_state REFUND)
      )]

      [(withdraw) (
          when (withdraw_req cur_state) (send cur_state withdraw)
      )]

      [(claim_refund) (
        when (claim_refund_req cur_state) (
          begin
          (set-field! last_refund_addr cur_state (message-sender (get_cur_message)))
          (set-field! refund_called cur_state #t)
          (set-field! last_refund_called cur_state #t)
          (interpret
            (external_call 
              (refund (message-sender (get_cur_message)) (send cur_state refund_deposit (message-sender (get_cur_message))))
              (get_cur_message)
            )
          )
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
      [(refund address value) (
        send cur_state transfer_value CONTRACT_ID address value
      )]

      ; Calls
      [(internal_call callable message) (
        begin
        (set! cur_state (send cur_state deepcopy))
        (set-field! cur_message cur_state message)
        (set-field! last_refund_called cur_state #f)
        (interpret callable)
      )]

      [(external_call callable message) (
        begin
        (set-field! cur_message cur_state message)
        (if (get-field external_call_failed cur_state)
            (set-field! external_call_failed cur_state #f)
            (interpret callable)
        )
      )]

      ; Utils
      [(set_overflow_time) (
        set-field! now cur_state (+ CLOSE_TIME 1)
      )]

      [(set_external_call_failed) (
        set-field! external_call_failed cur_state #t
      )]

      [_ p]
))


(define (exec calls msgs) (
  begin
  (init_state)
  (for ([c calls] [m msgs]) (
    interpret (internal_call (c) m)
  ))
  (print_states cur_state)
  (check_req cur_state)
))


; Normal Trace

; (define normal (exec 
;   (list invest set_overflow_time close_sale claim_refund)
;   (list (message USER_ID HALF_GOAL) default_message default_message (message USER_ID 0))
; ))


(define bug_trace1 (exec
  (list invest set_overflow_time set_external_call_failed close_sale claim_refund)
  (list (message USER_ID HALF_GOAL) default_message default_message default_message (message USER_ID 0))
))

; (pretty-print normal)
(pretty-print bug_trace1)
