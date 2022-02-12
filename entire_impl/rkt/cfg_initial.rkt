#lang racket

(require
    json
    racket/sandbox
    typed/racket/base
)

(provide gen-cfg-json count-reg)

(define vandal-timeout 120)

;; CFG generation starts
(define (gen-cmd cfg-file-loc) (string-append "python3 smart_opt.py " cfg-file-loc))
(define (gen-cfg-str cfg-file-loc) (
    with-handlers ([(lambda (v) #t) (lambda (v) 'timeout)])
    (call-with-limits vandal-timeout #f 
        (lambda () (with-output-to-string (lambda() (system (gen-cmd cfg-file-loc)))))
    )
))

(define (gen-cfg-json cfg-file-loc) (
  let ([cfg-str (gen-cfg-str cfg-file-loc)]) 
  (when (equal? cfg-str 'timeout)  (assert #f (string-append "vandal timeout in 2 mins!!!-->" cfg-file-loc)))
  (string->jsexpr cfg-str)
))

;; CFG generation ends

(define (count-reg cfg-json) (
    let ([max-regs 0])
    (for ([blk-json (hash-ref cfg-json `blocks)])
    	(for ([cur-inst (hash-ref blk-json `insts)])
            (for ([var (filter (lambda (str-arg) (string-prefix? str-arg "V")) (string-split cur-inst))])
                (when (< max-regs (string->number (substring var 1)))
                    (set! max-regs (string->number (substring var 1)))))
        )
    )
    max-regs
  )
)
