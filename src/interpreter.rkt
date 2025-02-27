#lang racket

;; Require the parser from a separate file, "simpleParser.rkt"
(require "simpleParser.rkt")

;; Provide (export) all definitions made in this file
(provide (all-defined-out))

;; `get-symbol` extracts the "symbol" from a list (like '(+ 1 2)), which is
;; stored in the car position ('+' in this example).
(define get-symbol car)

;; The initial state is an empty list (this is abstracted so it can be changed).
(define (get-initial-state)
  null)

;; `var-declared?` checks if a variable has been declared in the current
;; `state`. The `state` is a list of bindings, each binding being '(var-name
;; value). We use `index-where` to find the index of the binding for `var`. If
;; `index-where` returns a number, it means the variable is declared.
(define (var-declared? var state)
  (number? (index-where state (λ (binding) (eq? (car binding) var)))))

;; `get-pair-where-car-eq` retrieves all pairs in `lis` whose car equals `x`.
;; For instance, if `lis` is '((x 10) (y 20) (x 30)) and `x` is 'x,
;; it returns '((x 10) (x 30)).
(define (get-pair-where-car-eq lis x)
  (filter (lambda (v) (eq? (car v) x)) lis))

;; `get-var-value` returns the stored value for a variable `var` in `state`.
;; If the variable is not declared or no binding is found, it throws an error.
(define (get-var-value var state)
  (if (and (var-declared? var state) (not (null? (get-pair-where-car-eq state var))))
      (cadar (get-pair-where-car-eq state var))
      (error "variable used before declaration")))

;; `remove-var-bind` removes any binding from `state` that has the same car
;; (variable name) as the `binding` we pass in.
(define (remove-var-bind binding state)
  (filter (λ (existing-binding) (not (eq? (car existing-binding) (car binding)))) state))

;; `set-var-binding` puts a new binding (var, value) in `state`. If the var was
;; already declared, it removes the old binding first. Then it prepends the new
;; one.
(define (set-var-binding binding state)
  (cons binding (remove-var-bind binding state)))

;; We use `match-λ` to associate certain symbols with corresponding procedures
;; (as a dispatch table) for M_num-ops, M_bool-ops, and M_comp-ops.

;; Numeric operations. If the car of an expression is '+', that maps to
;; Racket's +, etc. For '-', if the second operand is null, we treat it as
;; unary negation; otherwise, binary subtraction.
(define M_num-ops
  (match-λ ['+ +]
           ['-
            (λ (a b)
              (if (null? b)
                  (- a)
                  (- a b)))]
           ['* *]
           ['/ quotient]
           ['% modulo]))

;; Boolean operations. '&& maps to (and a b), '|| maps to (or a b), '! is not.
;; For !, require two arguments but ignore the second one.
(define M_bool-ops
  (match-λ ['&& (λ (a b) (and a b))] ;;
           ['|| (λ (a b) (or a b))]
           ['! (λ (a _) (not a))]))

;; Comparison operations. '== is `eq?`, '!= is the negation of `eq?`, etc.
(define M_comp-ops
  (match-λ ['== eq?] ;;
           ['!= (λ (a b) (not (eq? a b)))]
           ['< <]
           ['> >]
           ['<= <=]
           ['>= >=]))

;; `M_state-stmt-list` processes a list of statements. If we run out of
;; statements, return the final `state`. Otherwise, evaluate the first
;; statement and recurse. `breaker` is a continuation used to handle 'return'
;; statements.
(define (M_state-stmt-list stmt-list state breaker)
  (if (null? stmt-list)
      state
      (M_state-stmt-list (cdr stmt-list) ;;
                         (M_state-stmt (car stmt-list) state breaker)
                         breaker)))

;; `M_state-stmt` matches on the type of statement (declaration, assignment,
;; while loop, conditional, and return) and dispatches to the appropriate
;; handler. If it's unrecognized, we error.
(define (M_state-stmt stmt state breaker)
  (match (car stmt)
    ['var (M_state-decl (cdr stmt) state)]
    ['= (M_state-assign (cdr stmt) state)]
    ['while (M_state-while stmt state breaker)]
    ['if (M_state-if stmt state breaker)]
    ['return (breaker (M_value (cadr stmt) state))]
    [_ (error "invalid statement type")]))

;; Declaration of a variable:
;;  1. If it's already declared, we error.
;;  2. If there's no initial value provided (aka the initial value is null, the
;;     empty list) just store the binding as (var null)).
;;  3. If there is an initial value, evaluate it and store that in the new
;;     state.
(define (M_state-decl binding state)
  (cond
    [(var-declared? (car binding) state) (error "variable redeclared")]
    [(null? (cdr binding)) (set-var-binding binding state)]
    [else (set-var-binding (list (car binding) (M_value (cadr binding) state)) state)]))

;; Assignment of a variable:
;;  1. If the var is declared, evaluate the expression and return the new
;;     state.
;;  2. Otherwise, error about an undeclared variable.
(define (M_state-assign binding state)
  (if (var-declared? (car binding) state)
      (set-var-binding (list (car binding) (M_value (cadr binding) state)) state)
      (error "variable use before declaration")))

;; While statement:
;;  1. Evaluate the condition (cadr).
;;  2. If true, execute the body (caddr) and loop again.
;;  3. If false, return the state as-is (loop ends).
(define (M_state-while while-stmt state break)
  (if (M_value (cadr while-stmt) state)
      (M_state-while while-stmt (M_state-stmt (caddr while-stmt) state break) break)
      state))

;; If statement:
;;  1. Evaluate the condition (cadr).
;;  2. If true, evaluate and return the state after the "then" branch (caddr).
;;  3. Else if there's an else branch (length is 4), evaluate "else" branch (cadddr).
;;  4. Otherwise, do nothing and return state.
(define (M_state-if if-stmt state break)
  (cond
    [(M_value (cadr if-stmt) state) (M_state-stmt (caddr if-stmt) state break)]
    [(eq? 4 (length if-stmt)) (M_state-stmt (cadddr if-stmt) state break)]
    [else state]))

;; `M_value-match-helper` is a small helper that:
;;  1. Gets the appropriate operator procedure from `op_func_getter`.
;;  2. Evaluates each of the operands using `M_value` to ensure they are fully
;;     processed and ready for use by the operator function.
;;  3. Applies the operator to those mapped results.
(define (M_value-match-helper op_func_getter expr state)
  ((op_func_getter (car expr)) (M_value (cadr expr) state)
                               (M_value (caddr (append expr (list null))) state)))

;; `M_value` evaluates an expression with respect to the given `state`.
(define (M_value expr state)
  (cond
    ;; Booleans
    [(eq? expr 'true) #t]
    [(eq? expr 'false) #f]
    ;; `M_value-match-helper` should always call its func with two "evaluated"
    ;; arguments, so we return null if we are given null (and stop recursing)
    ;; to allow for our two-argument ! (negation).
    [(null? expr) null]
    ;; Numbers
    [(number? expr) expr]
    ;; Symbols (variables)
    [(symbol? expr)
     (if (var-declared? expr state)
         (get-var-value expr state)
         (error "variable used before declaration"))]
    ;; Algebraic operations
    [(member (car expr) '(+ - * / %)) (M_value-match-helper M_num-ops expr state)]
    ;; Comparison operations
    [(member (car expr) '(== !=)) (M_value-match-helper M_comp-ops expr state)]
    [(member (car expr) '(>= <= < >)) (M_value-match-helper M_comp-ops expr state)]
    ;; Boolean operations
    [(member (car expr) '(&& || !)) (M_value-match-helper M_bool-ops expr state)]))

;; `output-remap` sanitizes the output.
(define (output-remap output)
  (match output
    [#t 'true]
    [#f 'false]
    [_ output]))

;; `interpret` is the main function:
;;  1. It parses the input file (or string) into a list of statements using `parser`.
;;  2. It calls `call/cc` to capture a continuation `breaker` used to exit early upon 'return'.
;;  3. It processes each statement, starting with an empty state (`'()`).
;;  4. Finally, we remap the final result to a more human-friendly output.
(define interpret
  (λ (file)
    (output-remap (call/cc (λ (breaker)
                             (M_state-stmt-list (parser file) (get-initial-state) breaker))))))

;; Read a line from standard input and interpret it immediately.
(interpret (read-line))

;; Example of how you'd call it on a file (commented out for now):
;; (interpret "./tests/input/test27_in")
