#lang rosette

(require
  "./rkt/solidity/solidity-parser.rkt"
  "./rkt/solidity/solidity-printer.rkt"
  "./rkt/solidity/solidity-machine.rkt"
  "./rkt/solidity/solidity-interpret.rkt"
  "./rkt/cfg_initial.rkt"
)

(require errortrace)

;; Base settings
(require rosette/solver/smt/boolector)
(current-solver (boolector #:logic 'QF_UFBV))

(define synthesis-timeout 360)
(define BV? (bitvector 256))
(current-bitwidth #f)
(define t1 (current-inexact-milliseconds))

(define crowdsale_loc "./contract/crowdsale.bytecode")
(define escrow_loc "./contract/escrow.bytecode")


(define machine (new solidity-machine% [config (+ 1 max-regs)]))
(define parser (new solidity-parser%))
(define printer (new solidity-printer% [machine machine]))

;; define number of bits used for generating random test inputs
(define test-bit 4)

(define (get-sym-func bit)
  (lambda
      (#:min [min-v #f] #:max [max-v #f] #:const [const #f])
    (define-symbolic* var BV?)
    var)
  )

;; create random input state
(define input-state (send machine get-state (get-sym-func test-bit)))
;; define our own input test, but memory content is random
;; modify program state to match your program state structure
;;#;(define input-state (progstate (vector ?)
; (new memory-racket% [get-fresh-val (get-rand-func test-bit)])))

(define simulator-rosette (new solidity-simulator-rosette% [machine machine]))
; (define synth-res (call-with-limits synthesis-timeout #f
;         (lambda () (send simulator-rosette init-analyzer cfg-file-loc parser printer cfg-json input-state))))

(define t2 (current-inexact-milliseconds))
(pretty-display `(Running time ,(- t2 t1)))
