#lang rosette/safe

(require 
  rosette/lib/destruct
  "./state.rkt"
)

(provide (all-defined-out))


(struct tm_pred () #:transparent)
(struct previous tm_pred (formula state) #:transparent)
(struct since tm_pred (formula state given_state) #:transparent)

(struct once tm_pred (formula state) #:transparent)
(struct always tm_pred (formula state) #:transparent)
(struct still tm_pred (formula state) #:transparent)

(struct t_and (x y) #:transparent)
(struct t_nand (x y) #:transparent)
(struct t_or (x y) #:transparent)

(define (eval-temp-expr _expr) (
  destruct _expr
    [(previous formula state) (
      if (null? (get-last state))
      #f
      (eval-temp-expr (formula (get-last state)))
    )]

    ; Not check whether the given state is non-moment
    [(since formula state given_state) (
      or
      (eval-temp-expr (formula state))
      (if (null? (get-last state))
        #f
        (if (or (null? given_state) (not (equal? (get-last given_state) state)))
          (eval-temp-expr (since formula (get-last state) given_state))
          #f
        )
      )
    )]

    [(once formula state) (
      eval-temp-expr (since formula state null)
    )]

    [(always formula state) (
      and
      (eval-temp-expr (formula state))
      (if (null? (get-last state))
        #t
        (eval-temp-expr (always formula (get-last state)))
      )
    )]

    [(t_and x y) (
      and
      (eval-temp-expr x)
      (eval-temp-expr y)
    )]

    [(t_nand x y) (
      nand
      (eval-temp-expr x)
      (eval-temp-expr y)
    )]

    [(t_or x y) (
      and
      (eval-temp-expr x)
      (eval-temp-expr y)
    )]

    [_ (
      begin
      ; (pretty-print _expr)
      _expr
    )]
))