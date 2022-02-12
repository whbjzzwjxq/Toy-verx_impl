#lang rosette/safe

(require
  "./state.rkt"
)

(define (sym-user) (define-symbolic* user integer?) user)
(define (sym-value) (define-symbolic* value integer?) value)
(define (symbolic-msg) (message (sym-user) (sym-value)))




