#lang rosette

(require
  "./state.rkt"
  "./interpret.rkt"
  racket/set
)

; add a while construct to Racket:
(define-syntax while 
  (syntax-rules ()
    [(_ cond body ...)
      (letrec [
        (loop (λ () (when cond body ... (loop))))]
        (loop)
      )
    ]
  )
)

(define (sym-value) (define-symbolic* value integer?) value)

(define (user-msg) (
  let ([value (sym-value)]) (
    begin
    (assume (>= value 0))
    (assume (<= value ALL_VALUE))
    (message USER_ID value)
  )
))

(define (owner-msg) (message OWNER_ID 0))

(define states-set (list init-state))

(define (is-in-list list value)
  (cond
    [(empty? list) #f]
    [(state-eq? (first list) value) #t]
    [else (is-in-list (rest list) value)]
  )
)

(define (add-state! state) (
  when (is-in-list states-set state)
  (set! states-set (append states-set state))
))
