;; Wolf Mermelstein (wsm32) and Christopher Danner (cld99)
;; 04/13 2025
;; CSDS345 Spring 2025

#lang racket
(require racket/cmdline)

;; Require the parser from a separate file, "simpleParser.rkt"
(require "classParser.rkt")

;; Provide (export) all definitions made in this file
(provide (all-defined-out))

;; Returns func applied to something, or null if that thing was null
(define (optional-apply f x)
  (if (null? x)
      null
      (f x)))

;; Flattens one layer
(define (flatten-state lst)
  (apply append lst))

;; The name of the entrypoint
(define (get-entrypoint-name)
  'main)

;; Prepend to the list
(define prepend cons)

(define recursion-tail cdr)

;; Get the head of the stuff
(define recursion-head car)

;; Get the function's casual params (actual params)
(define get-casual-params cddr)

;; The params for the entrypoint
(define (get-entrypoint-params)
  null)

;; Add a new layer to the state
(define (add-state-layer state (lis null))
  (cons lis state))

;; `get-symbol` extracts the "symbol" from a list (like '(+ 1 2)), which is
;; stored in the car position ('+' in this example).
(define (get-expr-symbol expr)
  (car expr))

;; `get-operand` gets all operands of a list
(define get-operands cdr)

;; `get-operand-1` gets the first operand of a list that looks like
;; '(== 5 2) would provide 5.
(define (get-operand-1 expr)
  (cadr expr))

;; `get-operand-2` gets the first ope"ESNext", rand of a list that looks like
;; '(== 5 2) would provide 2.
(define (get-operand-2 expr)
  (caddr expr))

;; `get-operand-3` gets the first operand of a list that looks like
;; '(if (== 5 2) (= x 5) (= y 2)) would return '(= y 2).
(define (get-operand-3 expr)
  (cadddr expr))

(define (get-field-defaults closure)
  (car closure))

(define (get-functions closure)
  (cadr closure))

(define (get-superclass closure)
  (caddr closure))

; closure:
; (
;   <formal param list>,
;   <function body>,
;   <function that creates env>
; )

;; `get-function-body` gets the function body from the closure
(define get-function-body cadr)

;; `get-env-getter` gets the environment creation function from the closure
(define get-env-getter caddr)

;; `get-static-type` gets the static class associated with the function
(define get-static-type cadddr)

;; `get-empty-state` returns an empty state
(define (get-empty-state)
  (list null))

;; `get-initial-state` returns the initial state for the interpreter, which
;; includes all of the "global" declarations
(define (get-initial-state outer-stmt-list)
  (M_state-stmt-list outer-stmt-list
                     (get-empty-state)
                     (λ (_value _state) (illegal-return-exception))
                     break-exception
                     continue-exception
                     (λ (_state _exception) (uncaught-exception-exception))
                     null
                     null
                     null))

;; `get-binding-unevaluated-value` gets the unevaluated expression list that is
;; the cadr of the binding
(define (get-binding-unevaluated-value binding)
  (cadr binding))

(define (get-evaled-binding binding state return except compile-type runtime-type this)
  (list (get-binding-name binding)
        ((M_value (get-binding-unevaluated-value binding)
                  state
                  return
                  except
                  compile-type
                  runtime-type
                  this))))

;; `get-binding-name` gets the name of a binding pair, which is its car.
(define get-binding-name car)

;; `get-latest-scope` gets the latest scope which is the leftmost scope.
(define get-latest-scope car)

;; `get-earlier-scopes` gets the earlier scopes which is the right of the leftmost scope.
(define get-earlier-scopes cdr)

;; `restore-state` restores the state layers to a specified state.
(define (restore-state _new-state old-state)
  old-state)

;; `var-used-before-dec-error` raises an error saying that a variable was used
;; before it was declared.
(define (var-used-before-dec-error msg)
  (error (string-append "variable used before declaration " (~a msg))))

;;`break-exception` is used to signal a break from a while loop.
(define (break-exception . _args)
  (error "broke outside while loop"))

;; `continue-exception` used to signal continuation to the next iteration of a
;; while loop.
(define (continue-exception . _args)
  (error "continued outside while loop"))

(define (illegal-return-exception)
  (error "illegal return"))
(define (uncaught-exception-exception)
  (error "uncaught exception"))

;; `var-declared?` checks if a variable has been declared in the current
;; `state`. The `state` is a list of bindings, each binding being '(var-name
;; value). We use `index-where` to find the index of the binding for `var`. If
;; `index-where` returns a number, it means the variable is declared.
(define (var-declared? var state)
  (number? (index-where (flatten-state state) (λ (binding) (eq? (get-binding-name binding) var)))))

;; `var-declared-in-scope?` checks if a variable has been declared in the current
;; scope represented by `state`. The `state` is a list of bindings, where each binding
;; is structured as '(var-name value). This function uses `index-where` to search
;; through the flattened state to find the index of the binding for the specified `var`.
;; If `index-where` returns a number, it indicates the variable is declared in the current scope.
(define (var-declared-in-scope? var state)
  (var-declared? var (list (get-latest-scope state))))

;; `get-pair-where-car-eq` retrieves all pairs in `lis` whose car equals `x`.
;; For instance, if `lis` is '((x 10) (y 20) (x 30)) and `x` is 'x,
;; it returns '((x 10) (x 30)).
(define (get-pair-where-car-eq lis x)
  (filter (λ (v) (eq? (car v) x)) lis))

;; `get-var-value` returns the stored value for a variable `var` in `state`.
;; If the variable is not declared or no binding is found, it throws an error.
(define (get-var-value var state)
  ;; Check to see if it is declared, then check to see if it is a pair
  (if (and (var-declared? var state)
           (not (null? (unbox (cadar (get-pair-where-car-eq (flatten-state state) var))))))
      ;; If it is a pair and declared, then return the value
      (unbox (cadar (get-pair-where-car-eq (flatten-state state)
                                           var))) ; the car of the cdr of the car is the binding value
      ;; Otherwise, throw an exception
      (var-used-before-dec-error var)))

;; `set-var-binding` updates an existing binding (var, value) in `state`.
;; If the <binding> is a single-element list (not a pair), this errors (that is
;; the purpose of add-var-binding)
;; `set-var-binding` updates an existing binding (var, value) in `state`.
(define (set-var-binding! binding state)
  (cond
    ;; we've recursed through all scopes without finding the variable
    [(null? state) (error (string-append "variable not declared: " (~a (get-binding-name binding))))]
    [(var-declared? (get-binding-name binding) (list (get-latest-scope state)))
     (begin
       (set-box! (cadar (get-pair-where-car-eq (get-latest-scope state) (get-binding-name binding)))
                 (get-binding-unevaluated-value binding))
       state)]
    [else (cons (get-latest-scope state) (set-var-binding! binding (get-earlier-scopes state)))]))

;; `add-var-bindings` zips and adds many bindings to the state.
(define (add-var-bindings keys values state (error-message "keys.length != values.length"))
  (cond
    [(and (null? keys) (null? values)) state]
    [(xor (null? keys) (null? values)) (raise error-message)]
    [else
     (add-var-bindings (recursion-tail keys)
                       (recursion-tail values)
                       (add-var-binding (list (recursion-head keys) (recursion-head values))
                                        state))]))

;; `add-var-binding` puts a new binding (var, value) in `state`.
(define (add-var-binding binding state)
  (cons (cons (if (null? (cdr binding))
                  (list (get-binding-name binding) (box null))
                  (list (get-binding-name binding) (box (cadr binding))))
              (get-latest-scope state))
        (get-earlier-scopes state)))

;; `M_state-stmt-list` processes a list of statements. If we run out of
;; statements, return the final `state`. Otherwise, evaluate the first
;; statement and recurse.
(define (M_state-stmt-list stmt-list
                           state
                           return
                           break
                           continue
                           except
                           compile-type
                           runtime-type
                           this)
  (if (null? stmt-list)
      state
      (M_state-stmt-list (recursion-tail stmt-list) ;;
                         (M_state-stmt (recursion-head stmt-list)
                                       state
                                       return
                                       break
                                       continue
                                       except
                                       compile-type
                                       runtime-type
                                       this)
                         return
                         break
                         continue
                         except
                         compile-type
                         runtime-type
                         this)))

;; `M_state-block` adds a new layer to the `state` and processes a block of
;; statements.
(define (M_state-block stmt-list
                       state
                       return
                       break
                       continue
                       except
                       compile-type
                       runtime-type
                       this
                       (push-new-state-level #t))
  (get-earlier-scopes (M_state-stmt-list stmt-list
                                         (if push-new-state-level
                                             (add-state-layer state)
                                             state)
                                         return
                                         (λ (state) (break (get-earlier-scopes state)))
                                         (λ (state) (continue (get-earlier-scopes state)))
                                         (λ (state exception)
                                           (except (get-earlier-scopes state) exception))
                                         compile-type
                                         runtime-type
                                         this)))

;; `M_state-function` handles function declarations.
(define (M_state-function name formal-params body state return except compile-type runtime-type this)
  (letrec (;; to define the function with access to itself
           [self
            (list
             (prepend 'this formal-params)
             body
             (λ (calling-state casual-params)
               (add-var-bindings
                (append (prepend 'this formal-params) (list name))
                (append
                 (map (λ (param)
                        (M_value param calling-state return except compile-type runtime-type this))
                      (prepend runtime-type casual-params))
                 (list self))
                (add-state-layer state)))
             compile-type)])
    (M_state-decl (list name self) state return except compile-type runtime-type this #f)))

;; `M_state-func-invoke` handles function invocations
(define (M_state-func-invoke function-name
                             state
                             casual-params
                             return
                             except
                             compile-type
                             runtime-type
                             this)
  (let ([function (get-var-value function-name state)])
    (restore-state
     (M_state-block (get-function-body function)
                    ((get-env-getter function) state casual-params)
                    (λ (to-return new-state) (return to-return (restore-state new-state state)))
                    break-exception
                    continue-exception
                    (λ (new-state exception) (except (restore-state new-state state) exception))
                    compile-type
                    runtime-type
                    this
                    #f)
     state)))

(define (M_state-class name extends body state)
  (add-var-binding
   (list
    name
    (list
     (add-state-layer (optional-apply (λ (extends) (get-field-defaults (get-var-value extends state)))
                                      extends)
                      (map get-operands ; List of list of bindings for compat with our state functions
                           (filter (λ (stmt) (eq? (get-operand-1 stmt) 'var)) body)))
     (M_state-stmt-list
      (filter
       (λ (stmt) ; Extract all the functions (and create the function closures via get-initial-state)
         (eq? (get-expr-symbol stmt) 'static-function))
       body)
      (if (null? extends)
          (get-empty-state)
          (get-functions (get-var-value extends state) extends))
      (λ (_value _state) (illegal-return-exception))
      break-exception
      continue-exception
      (λ (_state _exception) (uncaught-exception-exception))
      null
      null
      null)
     extends ; Extends type
     ))
   state))

;; `M_state-stmt` matches on the type of statement (declaration, assignment,
;; while loop, conditional, and return) and dispatches to the appropriate
;; handler. If it's unrecognized, we error.
(define (M_state-stmt stmt state return break continue except compile-type runtime-type this)
  (match (get-expr-symbol stmt)
    ['= (M_state-assign (get-operands stmt) state return except compile-type runtime-type this)]
    ['var (M_state-decl (get-operands stmt) state return except compile-type runtime-type this)]
    ['class
     (M_state-class (get-operand-1 stmt)
                    (optional-apply get-operand-1 (get-operand-2 stmt))
                    (get-operand-3 stmt)
                    state)]
    ['static-function
     (M_state-function ;;
      (get-operand-1 stmt)
      (get-operand-2 stmt)
      (get-operand-3 stmt)
      state
      return
      except
      compile-type
      runtime-type
      this)]
    ['funcall
     (if (eq? (get-expr-symbol (get-operand-1 stmt) 'dot))
         (M_state-func-invoke (get-operand-2 (get-operand-1 stmt))
                              state
                              (get-casual-params stmt)
                              (λ (_result state) (return state))
                              except
                              compile-type
                              runtime-type
                              (get-operand-1 (get-operand-1 stmt)))
         (call/cc (λ (return)
                    (M_state-func-invoke (get-operand-1 stmt)
                                         state
                                         (get-casual-params stmt)
                                         (λ (_result state) (return state))
                                         except
                                         compile-type
                                         runtime-type
                                         this))))]
    ['return
     (return (M_value (get-operand-1 stmt) state return except compile-type runtime-type this) state)]
    ['break (break state)]
    ['continue (continue state)]
    ['while
     (call/cc
      (λ (break)
        (M_state-while stmt state return break continue except compile-type runtime-type this)))]
    ['if (M_state-if stmt state return break continue except compile-type runtime-type this)]
    ['throw
     (except state (M_value (get-operand-1 stmt) state return except compile-type runtime-type this))]
    ['try
     (M_state-try (get-operand-1 stmt)
                  (get-operand-2 stmt)
                  (get-operand-3 stmt)
                  state
                  return
                  break
                  continue
                  except
                  compile-type
                  runtime-type
                  this)]
    ['begin
     (M_state-block (cdr stmt) state return break continue except compile-type runtime-type this)]
    [_ (error "invalid statement type")]))

;; `M_state-decl` handles variable declarations.
;;
;; Declaration of a variable:
;;  1. If it's already declared, we error.
;;  2. If there's no initial value provided (aka the initial value is null, the
;;     empty list) just store the binding as (var null)).
;;  3. If there is an initial value, evaluate it and store that in the new
;;     state.
(define (M_state-decl binding state return except compile-type runtime-type this (evaluate #t))
  (cond
    [(var-declared-in-scope? (get-binding-name binding) state)
     (error (string-append "variable redeclared: " (~a (get-binding-name binding))))]
    [(null? (cdr binding)) (add-var-binding binding state)]
    [evaluate
     (add-var-binding (list (get-binding-name binding)
                            (M_value (get-binding-unevaluated-value binding)
                                     state
                                     return
                                     except
                                     compile-type
                                     runtime-type
                                     this))
                      state)]
    [else (add-var-binding binding state)]))

;; (define (M_state-block stmt-list state return break continue except)
;;   (get-earlier-scopes (M_state-stmt-list stmt-list
;;                                          (add-state-layer state)
(define (M_state-try try-block
                     catch-stmt
                     finally-stmt
                     state
                     return
                     break
                     continue
                     except
                     compile-type
                     runtime-type
                     this)
  (letrec ([call-with-finally (λ (state)
                                (M_state-finally finally-stmt
                                                 state
                                                 return
                                                 break
                                                 continue
                                                 except
                                                 compile-type
                                                 runtime-type
                                                 this))]
           [return-with-finally (λ (to-return state) (return to-return (call-with-finally state)))]
           [jump-with-finally (λ (function)
                                (λ (state . args) (apply function (call-with-finally state) args)))])
    (M_state-finally
     finally-stmt ;; Finally statement
     (call/cc
      (λ (handler) ;; The new state post running the try block
        (M_state-block
         try-block
         state
         return-with-finally
         (jump-with-finally break)
         (jump-with-finally continue)
         ;; If we encounter an error then we fall back to the catch and use the
         ;; state that that gives us (we call handler with the new state).
         (jump-with-finally
          (λ (new-state ;; if it fails then it gives us the state that it got up to
              exception) ;; the thing that it failed with that we have to handle in M_state-catch
            (handler (M_state-catch catch-stmt
                                    new-state
                                    return-with-finally
                                    (jump-with-finally break)
                                    (jump-with-finally continue)
                                    (jump-with-finally except)
                                    exception
                                    compile-type
                                    runtime-type
                                    this))))
         compile-type
         runtime-type
         this)))
     return
     break
     continue
     except
     compile-type
     runtime-type
     this)))

(define (M_state-catch
         stmt ; could be '()' or 'catch (e) {}'
         state
         return
         break
         continue ;
         except
         ;; if the catch is emtpy or it errors again then we want to propagate the exception
         exception
         compile-type
         runtime-type
         this)
  (if (null? stmt)
      (except state exception) ;; if there is no catch then we propagate the exception
      (M_state-block (get-operand-2 stmt)
                     (add-var-binding (list (get-binding-name (get-operand-1 stmt)) exception)
                                      (add-state-layer state))
                     return
                     break
                     continue
                     except
                     compile-type
                     runtime-type
                     this
                     #f)))

(define (M_state-finally stmt state return break continue except compile-type runtime-type this)
  (if (null? stmt)
      state
      (M_state-block (get-operand-1 stmt)
                     state
                     return
                     break
                     continue
                     except
                     compile-type
                     runtime-type
                     this)))

;; `M_state-assign` handles variable assignments.
;;
;; Assignment of a variable:
;;  1. If the var is declared, evaluate the expression and return the new
;;     state.
;;  2. Otherwise, error about an undeclared variable.
(define (M_state-assign binding state return except compile-type runtime-type this)
  (if (var-declared? (get-binding-name binding) state)
      (set-var-binding! (list (get-binding-name binding)
                              (M_value (get-binding-unevaluated-value binding)
                                       state
                                       return
                                       except
                                       compile-type
                                       runtime-type
                                       this))
                        state)
      (var-used-before-dec-error (get-binding-name binding))))

;; `M_state-while` handles while loops.
;;
;; While statement:
;;  1. Evaluate the condition (cadr).
;;  2. If true, execute the body (caddr) and loop again.
;;  3. If false, return the state as-is (loop ends).
(define (M_state-while while-stmt state return break continue except compile-type runtime-type this)
  (if (M_value (cadr while-stmt) state return except compile-type runtime-type this)
      (M_state-while while-stmt
                     (call/cc (λ (continue)
                                (M_state-stmt (get-operand-2 while-stmt)
                                              state
                                              return
                                              break
                                              continue
                                              except
                                              compile-type
                                              runtime-type
                                              this)))
                     return
                     break
                     continue
                     except
                     compile-type
                     runtime-type
                     this)
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
(define (M_state-if if-stmt state return break continue except compile-type runtime-type this)
  (cond
    [(M_value (get-operand-1 if-stmt) state return except compile-type runtime-type this)
     (M_state-stmt (get-operand-2 if-stmt)
                   state
                   return
                   break
                   continue
                   except
                   compile-type
                   runtime-type
                   this)]
    [(contains-else? if-stmt)
     (M_state-stmt (get-operand-3 if-stmt)
                   state
                   return
                   break
                   continue
                   except
                   compile-type
                   runtime-type
                   this)]
    [else state]))

;; `M_value-map-then-apply-operator` is a small helper that:
;;  1. Gets the appropriate operator procedure from `op_function_getter`.
;;  2. Evaluates each of the operands using `M_value` to ensure they are fully
;;     processed and ready for use by the operator function.
;;  3. Applies the operator to those mapped results.
(define (M_value-map-then-apply-operator op_function_getter
                                         expr
                                         state
                                         return
                                         except
                                         compile-type
                                         runtime-type
                                         this)
  ((op_function_getter (get-expr-symbol expr))
   (M_value (get-operand-1 expr) state return except compile-type runtime-type this)
   (M_value (get-operand-2 (append expr (list null)))
            state
            return
            except
            compile-type
            runtime-type
            this)))

(define (M_value-instance name state return except compile-type runtime-type this)
  (list name
        (map (λ (class-fields-layer)
               (map (λ (binding)
                      (get-evaled-binding binding state return except compile-type runtime-type this))
                    class-fields-layer))
             (get-field-defaults (get-var-value name state)))))

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
(define (M_value expr state return except compile-type runtime-type this)
  (cond
    ;; Booleans
    [(eq? expr 'true) #t]

    [(eq? expr 'false) #f]

    [(null? expr) null]

    ;; Numbers
    [(number? expr) expr]

    ;; Symbols (variables)
    [(symbol? expr)
     (if (var-declared? expr state)
         (get-var-value expr state)
         (var-used-before-dec-error expr))]

    [(eq? (get-expr-symbol expr) 'new)
     (M_value-instance (get-operand-1 expr) state return except compile-type runtime-type this)]

    ;; Algebraic operations
    [(member (get-expr-symbol expr) '(+ - * / %))
     (M_value-map-then-apply-operator M_num-ops
                                      expr
                                      state
                                      return
                                      except
                                      compile-type
                                      runtime-type
                                      this)]

    ;; Comparison operations
    [(member (get-expr-symbol expr) '(== !=))
     (M_value-map-then-apply-operator M_comp-ops
                                      expr
                                      state
                                      return
                                      except
                                      compile-type
                                      runtime-type
                                      this)]

    [(member (get-expr-symbol expr) '(>= <= < >))
     (M_value-map-then-apply-operator M_comp-ops
                                      expr
                                      state
                                      return
                                      except
                                      compile-type
                                      runtime-type
                                      this)]

    ;; Boolean operations
    [(member (get-expr-symbol expr) '(&& || !))
     (M_value-map-then-apply-operator M_bool-ops
                                      expr
                                      state
                                      return
                                      except
                                      compile-type
                                      runtime-type
                                      this)]

    ;; `M_value-match-helper` should always call its function with two "evaluated"
    ;; arguments, so we return null if we are given null (and stop recursing)
    ;; to allow for our two-argument ! (negation).
    ;; Functions
    [(eq? (get-expr-symbol expr) 'functionall)
     (call/cc (λ (return)
                (M_state-func-invoke (get-operand-1 expr)
                                     state
                                     (get-casual-params expr)
                                     (λ (result _state) (return result))
                                     except
                                     null
                                     null
                                     null)))]))

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
(define (interpret file class-name)
  (output-remap (call/cc (λ (return)
                           (M_state-func-invoke
                            (get-entrypoint-name)
                            (get-functions (get-var-value class-name
                                                          (get-initial-state (parser file))))
                            (get-entrypoint-params)
                            (λ (to-return _state) (return to-return))
                            (λ (_state _exception) (uncaught-exception-exception))
                            null
                            null
                            null)))))

(define (run-interpreter)
  (command-line #:program "interpreter"
                #:args (file-path [class-name ""])
                (interpret file-path (string->symbol class-name))))

(run-interpreter)
