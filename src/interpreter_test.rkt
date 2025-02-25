#lang racket

(require "mappings.rkt")

(define (assert condition message)
  (if (not condition)
      (begin
        (display "Assertion failed: ")
        (display message)
        (newline)
        #f)
      #t))

(define sample-state '((y 5) (b 2) (a 2)))

; M_int tests
(assert (eq? 5 (M_int 5 sample-state)) "result should be 5")
(assert (eq? 2 (M_int 'b sample-state)) "result should be value b=2")

; Test boolean AND (&&)
(assert (eq? #t ((M_bool_ops '&&) #t #t)) "true AND true should be true")

; Test boolean OR (||)
(assert (eq? #t ((M_bool_ops '||) #t #t)) "true OR true should be true")

; Test boolean NOT (!)
(assert (eq? #f ((M_bool_ops '!) #t)) "NOT true should be false")
