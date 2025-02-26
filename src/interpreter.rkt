#lang racket

(require "simpleParser.rkt")
(provide (all-defined-out))

(define (var-declared? var state)
  (number? (index-where state (λ (binding) (eq? (car binding) var)))))

(define (get-var-value var state)
  (let ([filtered-vars (filter (lambda (v) (eq? (car v) var)) state)])
    (if (and (var-declared? var state) (not (null? filtered-vars)))
        (cadar filtered-vars)
        (error "variable used before declaration"))))

(define (remove-var-bind binding state)
  (filter ;
   (λ (existing-binding) (not (eq? (car existing-binding) (car binding))))
   state))

(define (set-var-binding binding state)
  (cons binding (remove-var-bind binding state)))

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
           ['! (λ (a _) (not a))]))

(define M_comp-ops
  (match-λ ['== eq?] ;
           ['!= (λ (a b) (not (eq? a b)))]
           ['< <]
           ['> >]
           ['<= <=]
           ['>= >=]))

(define (M_state-stmt-list stmt-list state breaker)
  (if (null? stmt-list)
      state
      (M_state-stmt-list (cdr stmt-list) (M_state-stmt (car stmt-list) state breaker) breaker)))

(define (M_state-stmt stmt state breaker)
  (match (car stmt)
    ['var (M_state-decl (cdr stmt) state)]
    ['= (M_state-assign (cdr stmt) state)]
    ['while (M_state-while stmt state breaker)]
    ['if (M_state-if stmt state breaker)]
    ['return (breaker (M_value (cadr stmt) state))]
    [_ (error "invalid statement type")]))

(define (M_state-decl binding state)
  (cond
    [(var-declared? (car binding) state) (error "variable redeclared")]
    [(null? (cdr binding)) (set-var-binding binding state)]
    [else (set-var-binding (list (car binding) (M_value (cadr binding) state)) state)]))

(define (M_state-assign binding state)
  (if (var-declared? (car binding) state)
      (set-var-binding (list (car binding) (M_value (cadr binding) state)) state)
      (error "variable use before declaration")))

(define (M_state-while while-stmt state break)
  (if (M_value (cadr while-stmt) state)
      (M_state-while while-stmt (M_state-stmt (caddr while-stmt) state break) break)
      state))

(define (M_state-if if-stmt state break)
  (cond
    [(M_value (cadr if-stmt) state) (M_state-stmt (caddr if-stmt) state break)]
    [(eq? 4 (length if-stmt)) (M_state-stmt (cadddr if-stmt) state break)]
    [else state]))

(define (M_value-match-helper op_func_getter expr mapper state)
  ((op_func_getter (car expr)) (mapper (cadr expr) state) (mapper (caddr (append expr '(()))) state)))

(define (M_value expr state)
  (cond
    [(eq? expr 'true) #t]
    [(eq? expr 'false) #f]
    [(null? expr) '()]
    [(number? expr) expr]
    [(symbol? expr)
     (if (var-declared? expr state)
         (get-var-value expr state)
         (error "variable used before declaration"))]
    [(member (car expr) '(+ - * / %)) (M_value-match-helper M_num-ops expr M_value state)]
    [(member (car expr) '(== !=)) (M_value-match-helper M_comp-ops expr M_value state)]
    [(member (car expr) '(>= <= < >)) (M_value-match-helper M_comp-ops expr M_value state)]
    [(member (car expr) '(&& || !)) (M_value-match-helper M_bool-ops expr M_value state)]))

(define (output-remap output)
  (match output
    [#t "true"] ;
    [#f "false"]
    [_ output]))

(define interpret
  (λ (file)
    (output-remap (call/cc (λ (breaker) (M_state-stmt-list (parser file) '() breaker))) ;
                  )))

(interpret (read-line))
;; (interpret "./tests/input/test27_in")
