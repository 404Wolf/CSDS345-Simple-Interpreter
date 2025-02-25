- Need to return true and false rather than #t and #f
- Need to check for use before assignment
- Need to check for redeclaration
- Need to abstract accessing of statement elements
- Need to abstract state declaration```racket
(define (M_int expr state)
  (cond
    [(list? expr)
     ((M_num-ops (car expr)) ;
      (M_int (cadr expr) state)
      (M_int (caddr expr) state))]
    [number? expr]
    [else (get-var-value expr state)]))(define (M_bool bool state)
  (if (number? bool)
      bool
      (get-var-value state bool)))```