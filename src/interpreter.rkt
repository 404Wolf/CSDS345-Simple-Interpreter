#lang racket

(require "simpleParser.rkt")
(provide (all-defined-out))

(define (interpret filename)
  (parser (filename)))

;; (interpret read-line)

; Get all of the variable names
(define (var-names s)
  (map car s))

; Get all of the variable values
(define (var-values s)
  (map cdr s))

; Get the value of a variable in the variable mappings
(define (var-value s var)
  (if (member var (var-names s))
      (cadar (filter (lambda (v) (eq? (car v) var)) s))
      #f))

; Map an int expression into a number
(define (M_int x s)
  (if (number? x)
      x
      (var-value s x)))

; Map an number operator into a function
(define M_num_ops
  (match-λ ['+ +] ;
           ['- -]
           ['* *]
           ['/ /]
           ['% modulo]))

; Map an boolean operator into a function
(define M_bool_ops
  (match-λ ['&& (λ (a b) (and a b))] ;
           ['|| (λ (a b) (or a b))]
           ['! (λ (a) (not a))]))

; Map an bool comparison operator into a function
(define (M_comp_ops x)
  (('== eq?) ;
   ('!= (λ (a b) (not (eq? a b))))
   ('< <)
   ('> >)
   ('<= <=)
   ('>= >=)))
