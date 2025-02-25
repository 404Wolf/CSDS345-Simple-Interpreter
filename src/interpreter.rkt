#lang racket

(require "simpleParser.rkt")
(provide (all-defined-out))

(define (all-var-names state)
  (map car state))

(define (all-var-values state)
  (map cdr state))

(define (var-declared? var state)
  (number? (index-where state (λ (binding) (eq? (car binding) var)))))

(define (get-var-value state var)
  (if (member var (all-var-names state))
      (cadar (filter (λ (v) (eq? (car v) var)) state))
      #f))

(define (set-var-binding var val state)
  (cons (list var val) (remove var state (lambda (variable binding) (eq? variable (car binding))))))

(define (M_declaration name state)
  (cons state (list name null)))

(define (M_assignment name value state)
  (map (λ (pair)
         (if (eq? name (car pair))
             (list (name value))
             pair)
         state)))

(define (M_return expr breaker)
  (breaker (M_value expr)))

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

(define (M_state-stmt-list stmt-list state break)
  (if (null? stmt-list)
      state
      (match (caar stmt-list)
        ['var (M_state-stmt-list (cdr stmt-list) (M_state-decl (car stmt-list) state) break)]
        ['= (M_state-stmt-list (cdr stmt-list) (M_state-assign (car stmt-list) state) break)]
        ['while (M_state-stmt-list (cdr stmt-list) (M_state-while (car stmt-list) state break) break)]
        ['if (M_state-stmt-list (cdr stmt-list) (M_state-if (car stmt-list) state break) break)]
        ['return (break (M_value (cadar stmt-list) state))]
        [_ (error "invalid statement type")])))

(define M_state-decl
  (λ (declaration state)
    (if (eq? 3 (length declaration))
        (set-var-binding (cadr declaration) (M_value (caddr declaration) state))
        (set-var-binding (cadr declaration) null state))))

(define M_state-assign
  (λ (assignment state)
    (if (get-var-value (cadr assignment) state)
        (set-var-binding (cadr assignment) (M_value (caddr assignment) state) state)
        (error "variable use before declaration"))))

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
          [(regexp "[+-*/%]") (M_value-match-helper M_num-ops expr M_int state)] ;
          [(regexp "==|!=") (M_value-match-helper M_comp-ops expr M_value state)] ;
          [(regexp ">=|<=|<|>") (M_value-match-helper M_comp-ops expr M_int state)]
          [(regexp "&&|\\|\\|!") (M_value-match-helper M_bool-ops expr M_bool state)]
          [_ (printf "Working")]) ;(get_state_value expr state))())))
        (match expr
          [#t expr]
          [#f expr]
          [number expr]
          ['_ (printf "Working")])))) ;(get_state_value expr state))))))

(define interpret
  (λ (file)
    ;(printf (parser file))))
    (call/cc (λ (breaker) (M_state-stmt-list (parser file) '() breaker)))))

(interpret "./tests/input/test03_in")

;; (interpret read-line)
