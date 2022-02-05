#lang rosette

(require rosette/lib/destruct)
(require data/enumerate)


; Escrow
(define OPEN 'OPEN)
(define SUCCESS 'SUCCESS)
(define REFUND 'REFUND)
(define state OPEN)

(define TRANSFER_FAILED #t)

(define owner_id 0)
(define beneficiary_id 1)
(define user_a_id 2)
(define user_b_id 3)

;; Only 4 users
(define user_num 4)
(define deposits (make-vector user_num 0))
(define account (make-vector user_num overflow_invest))

; Crowdsale
(define startTime 0)
(define endTime 30)

(define goal 10000)
(define zero_invest 0)
(define half_invest (/ goal 2))
(define total_invest goal)
(define overflow_invest (+ goal 1))


(struct message (recipient sender value data gas) #:transparent)

(define (only_owner _message) (= (message-sender _message) owner_id))

(define (modify_vector vec pos mod) (
    vector-set! vec pos (mod (vector-ref vec pos))
))

(define (transfer_value address value internal) (
    (if internal
        (modify_vector account address (lambda (x) (+ x value)))
        (if TRANSFER_FAILED
            (set! TRANSFER_FAILED (not TRANSFER_FAILED))
            (modify_vector account address (lambda (x) (+ x value)))
        )
    )
))

(struct invest (message))
(struct close (message))
(struct refund (message))
(struct deposit (message address))
(struct withdraw (message))


(define-symbolic raised 0)

(define (interp p)
    (destruct p
      ; Escrow
      [(close _message) (
          (if (only_owner _message) (set! state SUCCESS))
      )]

      [(refund _message) (
          (if (only_owner _message) (set! state REFUND))
      )]

      [(deposit _message address) (
          modify_vector deposits address (lambda (x) (+ x (message-value _message)))
      )]

      [(withdraw _message) (
          (if (= state REFUND) )
      )]
      

      [(invest _message) (
          (if (< raised goal) (

          ))
      )]
      [(mul x y) (* x y)]
      [_ (assert #f "infeasible")]))

