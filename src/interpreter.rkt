;; Wolf Mermelstein (wsm32) and Christopher Danner (cld99)
;; 02/26 2025
;; CSDS345 Spring 2025

#lang racket

;; Require the parser from a separate file, "simpleParser.rkt"
(require "simpleParser.rkt")

;; Provide (export) all definitions made in this file
(provide (all-defined-out))

;; Flattens one layer
(define (flatten-state lst)
  (apply append lst))

;; Add a new layer to the state
(define (add-state-layer state)
  (cons null state))

;; `get-symbol` extracts the "symbol" from a list (like '(+ 1 2)), which is
;; stored in the car position ('+' in this example).
(define (get-expr-symbol expr)
  (car expr))

;; `get-operand-1` gets the first operand of a list that looks like
;; '(== 5 2) would provide 5.
(define (get-operand-1 expr)
  (cadr expr))

;; `get-operand-2` gets the first operand of a list that looks like
;; '(== 5 2) would provide 2.
(define (get-operand-2 expr)
  (caddr expr))

;; `get-operand-3` gets the first operand of a list that looks like
;; '(if (== 5 2) (= x 5) (= y 2)) would return '(= y 2).
(define (get-operand-3 expr)
  (cadddr expr))

;; `get-initial-state` returns the initial state for the interpreter, which is
;; a list containing the empty list '(()).
(define (get-initial-state)
  (list null))

; `get-binding-unevaluated-value` gets the unevaluated expression list that is
; the cadr of the binding
(define (get-binding-unevaluated-value binding)
  (cadr binding))

; `get-binding-name` gets the name of a binding pair, which is its car.
(define get-binding-name car)

; `get-latest-scope` gets the latest scope which is the leftmost scope.
(define get-latest-scope car)

; `get-earlier-scopes` gets the earlier scopes which is the right of the leftmost scope.
(define get-earlier-scopes cdr)

; `strip-off-catch-name-scope-block` gets the earlier scopes which is the right of the leftmost scope.
(define strip-off-catch-name-scope-block cdr)

;; `var-used-before-dec-error` raises an error saying that a variable was used
;; before it was declared.
(define (var-used-before-dec-error msg)
  (error (string-append "variable used before declaration" (~a msg))))

;; `var-declared?` checks if a variable has been declared in the current
;; `state`. The `state` is a list of bindings, each binding being '(var-name
;; value). We use `index-where` to find the index of the binding for `var`. If
;; `index-where` returns a number, it means the variable is declared.
(define (var-declared? var state)
  (number? (index-where (flatten-state state) (λ (binding) (eq? (get-binding-name binding) var)))))

;; `get-pair-where-car-eq` retrieves all pairs in `lis` whose car equals `x`.
;; For instance, if `lis` is '((x 10) (y 20) (x 30)) and `x` is 'x,
;; it returns '((x 10) (x 30)).
(define (get-pair-where-car-eq lis x)
  (filter (λ (v) (eq? (car v) x)) lis))

;; `get-var-value` returns the stored value for a variable `var` in `state`.
;; If the variable is not declared or no binding is found, it throws an error.
(define (get-var-value var state)
  (if (and (var-declared? var state) (not (null? (get-pair-where-car-eq (flatten-state state) var))))
      (cadar (get-pair-where-car-eq (flatten-state state) var))
      (var-used-before-dec-error var)))

;; `remove-var-binding` removes any binding from the currect scope of `state`
;; that has the same car (variable name) as the `binding` we pass in.
(define (remove-var-binding binding state)
  (filter (λ (existing-binding) ;
            (not (eq? (get-binding-name existing-binding) (get-binding-name binding))))
          state))

;; `set-var-binding` updates an existing binding (var, value) in `state`.
(define (set-var-binding binding state)
  (if (var-declared? (get-binding-name binding) (list (get-latest-scope state)))
      (cons (cons binding (remove-var-binding binding (get-latest-scope state)))
            (get-earlier-scopes state))
      (cons (get-latest-scope state) (set-var-binding binding (get-earlier-scopes state)))))

;; `add-var-binding` puts a new binding (var, value) in `state`. If the var was
;; already declared, it removes the old binding first. Then it prepends the new
;; one.
(define (add-var-binding binding state)
  (cons (cons binding (get-latest-scope state)) (get-earlier-scopes state)))

;; `M_state-stmt-list` processes a list of statements. If we run out of
;; statements, return the final `state`. Otherwise, evaluate the first
;; statement and recurse.
(define (M_state-stmt-list stmt-list state return break continue except)
  (if (null? stmt-list)
      state
      (M_state-stmt-list (cdr stmt-list) ;;
                         (M_state-stmt (car stmt-list) state return break continue except)
                         return
                         break
                         continue
                         except)))

;; `M_state-block` adds a new layer to the `state` and processes a block of
;; statements.
(define (M_state-block stmt-list state return break continue except)
  (get-earlier-scopes (M_state-stmt-list stmt-list
                                         (add-state-layer state)
                                         return
                                         (λ (state) (break (get-earlier-scopes state)))
                                         (λ (state) (continue (get-earlier-scopes state)))
                                         (λ (state exception)
                                           (except (get-earlier-scopes state) exception)))))

;; `M_state-stmt` matches on the type of statement (declaration, assignment,
;; while loop, conditional, and return) and dispatches to the appropriate
;; handler. If it's unrecognized, we error.
(define (M_state-stmt stmt state return break continue except)
  (match (get-expr-symbol stmt)
    ['var (M_state-decl (cdr stmt) state)]
    ['= (M_state-assign (cdr stmt) state)]
    ['return (return (M_value (get-operand-1 stmt) state))]
    ['break (break state)]
    ['continue (continue state)]
    ['while (call/cc (λ (break) (M_state-while stmt state return break continue except)))]
    ['if (M_state-if stmt state return break continue except)]
    ['throw (except state (get-operand-1 stmt))]
    ['try
     (M_state-try (get-operand-1 stmt)
                  (get-operand-2 stmt)
                  (get-operand-3 stmt)
                  state
                  return
                  break
                  continue
                  except)]
    ['begin (M_state-block (cdr stmt) state return break continue except)]
    [_ (error "invalid statement type")]))

;; `M_state-decl` handles variable declarations.
;;
;; Declaration of a variable:
;;  1. If it's already declared, we error.
;;  2. If there's no initial value provided (aka the initial value is null, the
;;     empty list) just store the binding as (var null)).
;;  3. If there is an initial value, evaluate it and store that in the new
;;     state.
(define (M_state-decl binding state)
  (cond
    [(var-declared? (get-binding-name binding) state)
     (error (string-append "variable redeclared: " (~a (car binding))))]
    [(null? (cdr binding)) (add-var-binding binding state)]
    [else
     (add-var-binding (list (get-binding-name binding)
                            (M_value (get-binding-unevaluated-value binding) state))
                      state)]))

;; (define (M_state-block stmt-list state return break continue except)
;;   (get-earlier-scopes (M_state-stmt-list stmt-list
;;                                          (add-state-layer state)
(define (M_state-try try-block catch-stmt finally-stmt state return break continue except)
  (M_state-finally
   finally-stmt ;; Finally statement
   (call/cc
    (λ (handler) ;; The new state post running the try block
      (M_state-block
       try-block
       state
       return
       break
       continue
       ;; If we encounter an error then we fall back to the catch and
       ;; use the state that that gives us (we call handler with the
       ;; new state).
       (λ (new-state ;; if it fails then it gives us the state that it got up to
           exception) ;; the thing that it failed with that we have to handle in M_state-catch
         (handler (M_state-catch catch-stmt new-state return break continue except exception)))))) ;
   return
   break
   continue
   except))

(define (M_state-catch
         stmt ; could be '()' or 'catch (e) {}'
         state
         return
         break
         continue ;
         except
         ;; if the catch is emtpy or it errors again then we want to propagate the exception
         exception)
  (if (null? stmt)
      (except state exception) ;; if there is no catch then we propagate the exception
      (M_state-block (get-operand-2 stmt)
                     (add-var-binding (list (get-binding-name (get-operand-1 stmt)) exception) state)
                     return
                     break
                     continue
                     except)))

(define (M_state-finally stmt state return break continue except)
  (if (null? stmt)
      state
      (M_state-block (get-operand-1 stmt) state return break continue except)))

;; `M_state-assign` handles variable assignments.
;;
;; Assignment of a variable:
;;  1. If the var is declared, evaluate the expression and return the new
;;     state.
;;  2. Otherwise, error about an undeclared variable.
(define (M_state-assign binding state)
  (if (var-declared? (get-binding-name binding) state)
      (set-var-binding (list (get-binding-name binding)
                             (M_value (get-binding-unevaluated-value binding) state))
                       state)
      (var-used-before-dec-error (get-binding-name binding))))

;; `M_state-while` handles while loops.
;;
;; While statement:
;;  1. Evaluate the condition (cadr).
;;  2. If true, execute the body (caddr) and loop again.
;;  3. If false, return the state as-is (loop ends).
(define (M_state-while while-stmt state return break continue except)
  (if (M_value (cadr while-stmt) state)
      (M_state-while
       while-stmt
       (call/cc (λ (continue)
                  (M_state-stmt (get-operand-2 while-stmt) state return break continue except)))
       return
       break
       continue
       except)
      state))

;; `contains-else?` checks if an if statement has an else branch.`
(define (contains-else? if-stmt)
  (eq? 4 (length if-stmt)))

;; `M_state-if` handles if statements.
;;
;; If statement:
;;  1. Evaluate the condition (cadr).
;;  2. If true, evaluate and return the state after the "then" branch (caddr).
;;  3. Else if there's an else branch (length is 4), evaluate "else" branch (cadddr).
;;  4. Otherwise, do nothing and return state.
(define (M_state-if if-stmt state return break continue except)
  (cond
    [(M_value (get-operand-1 if-stmt) state)
     (M_state-stmt (get-operand-2 if-stmt) state return break continue except)]
    [(contains-else? if-stmt)
     (M_state-stmt (get-operand-3 if-stmt) state return break continue except)]
    [else state]))

;; `M_value-match-helper` is a small helper that:
;;  1. Gets the appropriate operator procedure from `op_func_getter`.
;;  2. Evaluates each of the operands using `M_value` to ensure they are fully
;;     processed and ready for use by the operator function.
;;  3. Applies the operator to those mapped results.
(define (M_value-map-then-apply-operator op_func_getter expr state)
  ((op_func_getter (get-expr-symbol expr)) (M_value (get-operand-1 expr) state)
                                           (M_value (get-operand-2 (append expr (list null))) state)))

;; We use `match-λ` to associate certain symbols with corresponding procedures
;; (as a dispatch table) for M_num-ops, M_bool-ops, and M_comp-ops.
;;
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
         (var-used-before-dec-error expr))]
    ;; Algebraic operations
    [(member (get-expr-symbol expr) '(+ - * / %))
     (M_value-map-then-apply-operator M_num-ops expr state)]
    ;; Comparison operations
    [(member (get-expr-symbol expr) '(== !=)) (M_value-map-then-apply-operator M_comp-ops expr state)]
    [(member (get-expr-symbol expr) '(>= <= < >))
     (M_value-map-then-apply-operator M_comp-ops expr state)]
    ;; Boolean operations
    [(member (get-expr-symbol expr) '(&& || !))
     (M_value-map-then-apply-operator M_bool-ops expr state)]))

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
    (output-remap (call/cc (λ (return)
                             (M_state-stmt-list (parser file)
                                                (get-initial-state)
                                                return
                                                (λ (_state) (error "broke outside while loop"))
                                                (λ (_state) (error "continued outside while loop"))
                                                (λ (_state _exception)
                                                  (error "uncaught except"))))))))
(interpret (read-line))
