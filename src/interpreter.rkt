#lang racket

(require "simpleParser.rkt")
(provide (all-defined-out))

(define (all-var-names state)
  (map car state))

(define (all-var-values state)
  (map cdr state))

(define (var-declared? var state)
  (number? (index-where state (λ (binding) (eq? (car binding) var)))))

(define (get-var-value var state)
  (if (var-declared? var state)
      (cadar (filter (λ (v) (eq? (car v) var)) state))
      #f))

(define (remove-var-bind binding state)
  (filter ;
   (λ (existing-binding) (not (eq? (car existing-binding) (car binding))))
   state))

(define (set-var-binding binding state)
  (cons binding (remove-var-bind binding state)))

(define (M_declaration name state)
  (cons state (list name null)))

(define (M_assignment name value state)
  (map (λ (pair)
         (if (eq? name (car pair))
             (list (name value))
             pair)
         state)))

(define (M_int expr state)
  (cond
    [(list? expr)
     ((M_num-ops (car expr)) ;
      (M_int (cadr expr) state)
      (M_int (caddr expr) state))]
    [number? expr]
    [(var-declared? expr) (get-var-value state expr)]))

(define (M_bool bool state)
  (if (number? bool)
      bool
      (get-var-value state bool)))

(define M_num-ops
  (match-λ ['+ +] ;
           ['-
            (λ (a b)
              (if (null? b)
                  (- a)
                  (- a b)))]
           ['* *]
           ['/ quotient]
           ['% modulo]))

(define M_bool-ops
  (match-λ ['&& (λ (a b) (and a b))] ;
           ['|| (λ (a b) (or a b))]
           ['! (λ (a) (not a))]))

(define (M_comp-ops x)
  (('== eq?) ;
   ('!= (λ (a b) (not (eq? a b))))
   ('< <)
   ('> >)
   ('<= <=)
   ('>= >=)))

(define (M_state-stmt-list stmt-list state breaker)
  (if (null? stmt-list)
      state
      (match (caar stmt-list)
        ['var (M_state-stmt-list (cdr stmt-list) (M_state-decl (cdar stmt-list) state) breaker)]
        ['= (M_state-stmt-list (cdr stmt-list) (M_state-assign (cdar stmt-list) state) breaker)]
        ['while
         (M_state-stmt-list (cdr stmt-list) (M_state-while (car stmt-list) state breaker) breaker)]
        ['if (M_state-stmt-list (cdr stmt-list) (M_state-if (car stmt-list) state breaker) breaker)]
        ['return (breaker (M_value (cadar stmt-list) state))]
        [_ (error "invalid statement type")])))

(define (M_state-decl binding state)
  (if (null? (cdr binding))
      (set-var-binding binding state)
      (set-var-binding (list (car binding) (M_value (cadr binding) state)) state)))

(define (M_state-assign binding state)
  (if (var-declared? (car binding) state)
      (set-var-binding (list (car binding) (M_value (cadr binding) state)) state)
      (error "variable use before declaration")))

(define M_state-while
  (λ (while-stmt state break)
    (if (M_value (cadr while-stmt) state)
        (M_state-while while-stmt (M_state-stmt-list (caddr while-stmt) state break) break)
        state)))

(define M_state-if
  (λ (if-stmt state break)
    (cond
      [(M_value (cadr if-stmt) state) (M_state-stmt-list (caddr if-stmt) state break)]
      [(eq? 4 (length if-stmt)) (M_state-stmt-list (cadddr if-stmt) state break)]
      [else state])))

(define (M_value-match-helper op_func_getter expr mapper state)
  ((op_func_getter (car expr)) (mapper (cadr expr) state) (mapper (caddr expr) state)))

(define M_value
  (λ (expr state)
    (if (pair? expr)
        (match (car expr)
          [(pregexp "[+-*/%]") (M_value-match-helper M_num-ops expr M_int state)] ;
          [(pregexp "==|!=") (M_value-match-helper M_comp-ops expr M_value state)] ;
          [(pregexp ">=|<=|<|>") (M_value-match-helper M_comp-ops expr M_int state)]
          [(pregexp "&&|\\|\\|!") (M_value-match-helper M_bool-ops expr M_bool state)])
        (match expr
          [#t expr]
          [#f expr]
          [(? number?) expr]
          ['() '()]
          [_ (get-var-value expr state)]))))

(define interpret
  (λ (file)
    ;(printf (parser file))))
    (call/cc (λ (breaker) (M_state-stmt-list (parser file) '() breaker)))))

(interpret "./tests/input/test03_in")

;; (interpret read-line)
