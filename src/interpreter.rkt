#lang racket

(require "simpleParser.rkt")

(define interpret (lambda (filename) (parser (filename))))

;; (interpret read-line)

; Get all of the variable names
(define var-names (lambda (s) (map car s)))

; Get all of the variable values
(define var-values (lambda (s) (map cdr s)))

; Get the value of a variable in the variable mappings
(define var-value
  (lambda (s var)
    (if (member var (var-names s))
        (cadar (filter (lambda (v) (eq? (car v) var)) s))
        #f)))

(define M_int
  (lambda (int state)
    (cond
      [(member state) (list-ref (index-of (state int)))]
      [(number? int) int])))
