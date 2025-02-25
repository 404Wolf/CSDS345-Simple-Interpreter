#lang racket

(require "simpleParser.rkt")

(define interpret
  (lambda (file)
    ;(printf (parser file))))
    (call/cc (lambda (k) (M_state_stmt_list (parser file) '() k)))))

(define get_state_value
  (lambda (var state)
    (if (number? (index-where state (lambda (binding) (eq? (car binding) var))))
        (cadr (list-ref state (index-where state (lambda (binding) (eq? (car binding) var)))))
        (error "variable use before declaration"))))

(define get_state_declared
  (lambda (var state) (number? (index-where state (lambda (binding) (eq? (car binding) var))))))

(define set_state_binding
  (lambda (var val state)
    (cons (list var val)
          (remove var state (lambda (variable binding) (eq? variable (car binding)))))))

(define M_state_stmt_list
  (lambda (stmt-list state break)
    (cond
      [(null? stmt-list) state]
      [(eq? (caar stmt-list) '|var|)
       (M_state_stmt_list (cdr stmt-list) (M_state_decl (car stmt-list) state) break)]
      [(eq? (caar stmt-list) '=)
       (M_state_stmt_list (cdr stmt-list) (M_state_assign (car stmt-list) state) break)]
      [(eq? (caar stmt-list) '|while|)
       (M_state_stmt_list (cdr stmt-list) (M_state_while (car stmt-list) state break) break)]
      [(eq? (caar stmt-list) '|if|)
       (M_state_stmt_list (cdr stmt-list) (M_state_if (car stmt-list) state break) break)]
      [(eq? (caar stmt-list) '|return|) (break (M_value (cadar stmt-list) state))]
      [else (error "invalid statement type")])))

(define M_state_decl
  (lambda (declaration state)
    (if (eq? 3 (length declaration))
        (set_state_binding (cadr declaration) (M_value (caddr declaration) state))
        (set_state_binding (cadr declaration) null state))))

(define M_state_assign
  (lambda (assignment state)
    (if (get_state_declared (cadr assignment) state)
        (set_state_binding (cadr assignment) (M_value (caddr assignment) state) state)
        (error "variable use before declaration"))))

(define M_state_while
  (lambda (while-stmt state break)
    (if (M_value (cadr while-stmt) state)
        (M_state_while while-stmt (M_state_stmt_list (caddr while-stmt) state break) break)
        state)))

(define M_state_if
  (lambda (if-stmt state break)
    (cond
      [(M_value (cadr if-stmt) state) (M_state_stmt_list (caddr if-stmt) state break)]
      [(eq? 4 (length if-stmt)) (M_state_stmt_list (cadddr if-stmt) state break)]
      [else state])))

(define M_value
  (lambda (expr state)
    (if (pair? expr)
        (match (car expr)
          ['+ (+ (M_value (cadr expr) state) (M_value (caddr expr) state))]
          ['-
           (if (eq? 3 (length expr))
               (- (M_value (cadr expr) state) (M_value (caddr expr) state))
               (- 0 (M_value (cadr expr) state)))]
          ['* (* (M_value (cadr expr) state) (M_value (caddr expr) state))]
          ['/ (quotient (M_value (cadr expr) state) (M_value (caddr expr) state))]
          ['% (modulo (M_value (cadr expr) state) (M_value (caddr expr) state))]
          ['|==| (eq? (M_value (cadr expr) state) (M_value (caddr expr) state))]
          ['|!=| (not (eq? (M_value (cadr expr) state) (M_value (caddr expr) state)))]
          ['|>=| (>= (M_value (cadr expr) state) (M_value (caddr expr) state))]
          ['|<=| (<= (M_value (cadr expr) state) (M_value (caddr expr) state))]
          ['> (> (M_value (cadr expr) state) (M_value (caddr expr) state))]
          ['< (< (M_value (cadr expr) state) (M_value (caddr expr) state))]
          ['|&&| (and (M_value (cadr expr) state) (M_value (caddr expr) state))]
          ['|||| (or (M_value (cadr expr) state) (M_value (caddr expr) state))]
          ['! (not (M_value (cadr expr) state))]
          ['_ (printf "Working")]) ;(get_state_value expr state))))))
        (match expr
          [#t expr]
          [#f expr]
          [number expr]
          ['_ (printf "Working")])))) ;(get_state_value expr state))))))
