#lang eopl

;; lambda-exp : Var * Lc-exp -> Lc-exp
(define lambda-exp
  (lambda (bound-var body)
    (list 'lambda bound-var body)))

;; lambda-exp? : Lc-exp -> Bool
(define lambda-exp?
  (lambda (exp)
    (and (pair? exp)
         (eqv? (car exp) 'lambda))))

;; lambda-exp->bound-var : Lc-exp -> Var
(define lambda-exp->bound-var
  (lambda (exp) (cadr exp)))

;; lambda-exp->body : Lc-exp -> Lc-exp
(define lambda-exp->body
  (lambda (exp) (caddr exp)))
