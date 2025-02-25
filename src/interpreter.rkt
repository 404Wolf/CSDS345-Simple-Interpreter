#lang racket

(require "simpleParser.rkt")
(provide (all-defined-out))

(define % modulo)

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

; Get the next state as a result of evaluating a statement.
(define (M_state statement state breaker)
  (match statement
    ['= (M_assignment (cdr statement) state)] ;
    ['var (M_declaration (cdr statement) state)] ;
    ['return (M_return 'return (M_return M_return breaker))] ;
    ))

; Map an variable declaration and add it to the state (with no value). Returns
; the new state.
(define (M_declaration name state)
  (cons state (list name null)))

; Map an assignment statement. Updates the state with the new value at the
; entry for the name.
(define (M_assignment name value state)
  (map (λ (pair)
         (if (eq? name (car pair))
             (list (name value))
             pair)
         state)))

; Map an return statement. Calls breaker, returns nothing.
(define (M_return expr breaker)
  (breaker (M_value expr)))

; Map a value (either an int, bool, or variable). Returns the value.
(define (M_value value state)
  (or (map (lambda (f) (f value state)) (list M_int M_bool))))

; Map an int expression into a number
(define (M_int int state)
  (if (number? int)
      int
      (get_var_value state int)))

; Map an int expression into a number. Returns the number.
(define (M_bool bool state)
  (if (number? bool)
      bool
      (get_var_value state bool)))

; Map an number operator into a function. Returns the corresponding function.
(define M_num_ops
  (match-λ ['+ +] ;
           ['- -]
           ['* *]
           ['/ /]
           ['% %]))

; Map an boolean operator into a function. Returns the corresponding function.
(define M_bool_ops
  (match-λ ['&& (λ (a b) (and a b))] ;
           ['|| (λ (a b) (or a b))]
           ['! (λ (a) (not a))]))

; Map an bool comparison operator into a function. Returns the corresponding
; function.
(define (M_comp_ops x)
  (('== eq?) ;
   ('!= (λ (a b) (not (eq? a b))))
   ('< <)
   ('> >)
   ('<= <=)
   ('>= >=)))

(define (interpret filename)
  (call/cc (λ (breaker)
             (M_state ; main interpreter entrypoint
              (parser filename) ; entrypoint
              breaker ; call to return
              '() ; initial state
              ))))

(interpret "./input")

;; (interpret read-line)
