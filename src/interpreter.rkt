#lang racket

(require "simpleParser.rkt")
(provide (all-defined-out))

(define (interpret filename)
  (parser (filename)))

;; (interpret read-line)

; Get all of the variable names
(define (all-var-names state)
  (map car state))

; Get all of the variable values
(define (all-var-values state)
  (map cdr state))

; Get the value of a variable in the variable mappings
(define (get_var_value state var)
  (if (member var (all-var-names state))
      (cadar (filter (lambda (v) (eq? (car v) var)) state))
      #f))

; Map an return statement
(define (M_return expr breaker)
  (breaker (M_value expr)))

; Map a value (either an int, bool, or variable)
(define (M_value value state)
  (or (map (lambda (f) (f value state)) (list M_int M_bool))))

; Map an int expression into a number
(define (M_int int state)
  (if (number? int)
      int
      (get_var_value state int)))

; Map an int expression into a number
(define (M_bool bool state)
  (if (number? bool)
      bool
      (get_var_value state bool)))

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
