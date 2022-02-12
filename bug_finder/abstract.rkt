#lang rosette

(require
  "./state.rkt"
)

(provide (all-defined-out))

(define (bin_abstract op an1 an2)
  (apply set-union
    (for*/list ([s1 an1] [s2 an2])
      (op s1 s2)
    )
  )
)

(define (+/a an1 an2) (bin_abstract +/abstract an1 an2))
(define (-/a an1 an2) (+/a an1 (reverse-abstract an2)))
(define (all-preds) {set '-G+ -GOAL '- 0 '+ GOAL 'G+})

(define (get-abstract n)
  (cond
    [(< n -GOAL) {set '-G+}]
    [(= n -GOAL) {set -GOAL}]
    [(and (< n 0) (> n -GOAL)) {set '-}]
    [(= n 0) {set 0}]
    [(and (> n 0) (< n GOAL)) {set '+}]
    [(= n GOAL) GOAL]
    [(> n GOAL) {set 'G+}]
    [else (all-preds)]
  )
)

(define (reverse-abstract n) (
  (cond
    [(equal? n '-G+) 'G+]
    [(equal? n -GOAL) GOAL]
    [(equal? n '-) '+]
    [(equal? n 0) 0]
    [(equal? n '+) '-]
    [(equal? n GOAL) -GOAL]
    [(equal? n 'G+) '-G+]
  )
))

(define lt-zero-sets {set '-G+ -GOAL '-})
(define (lt-zero a) (set-member? lt-zero-sets a))

(define gt-zero-sets {set 'G+ GOAL '+})
(define (gt-zero a) (set-member? gt-zero-sets a))

(define (+/abstract s1 s2)
  (if (equal? s2 0)
    (+/abstract 0 s1)
    (match* (s1 s2)
      [('-G+ s2) #:when (lt-zero s2) {set '-G+}]
      [('-G+ '+) {set '-G+ -GOAL '-}]
      [('-G+ GOAL) {set '-G+ -GOAL '-}]
      [('-G+ 'G+) (all-preds)]

      [(-GOAL s2) #:when (lt-zero s2) {set '-G+}]
      [(-GOAL '+) {set '-}]
      [(-GOAL GOAL) {set 0}]
      [(-GOAL 'G+) {set '+ GOAL 'G+}]

      [('- '-G+) {set '-G+}]
      [('-  -GOAL) {set '-G+}]
      [('- '-) {set '-G+ -GOAL '-}]
      [('- '+) {set '- 0 '+}]
      [('- GOAL) {set '+}]
      [('- 'G+) {set '+ GOAL 'G+}]

      [(0 x) {set x}]

      [('+ '-G+) {set '-G+ -GOAL '-}]
      [('+ -GOAL) {set '-}]
      [('+ '-) {set '- 0 '+}]

      [('+ '+) {set '+ GOAL 'G+}]
      [('+ GOAL) {set 'G+}]
      [('+ 'G+) {set 'G+}]

      [(GOAL '-G+) {set '- -GOAL '-G+}]
      [(GOAL -GOAL) {set 0}]
      [(GOAL '-) {set '+}]
      [(GOAL s2) #:when (gt-zero s2) {set 'G+}]

      [('G+ '-G+) (all-preds)]
      [('G+ -GOAL) {set 'G+ GOAL '+}]
      [('G+ -) {set 'G+ GOAL '+}]

      [('G+ s2) #:when (gt-zero s2) {set 'G+}]
    )
  )
)
