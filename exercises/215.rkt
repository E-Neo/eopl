#lang eopl

;; Lc-exp ::= Identifier
;;        ::= (lambda (Identifier) Lc-exp)
;;        ::= (Lc-exp Lc-exp)

;; var-exp : Var -> Lc-exp
(define var-exp
  (lambda (var) var))

;; var-exp? : Lc-exp -> Bool
(define var-exp?
  (lambda (exp) (symbol? exp)))

;; var-exp->var : Lc-exp -> Var
(define var-exp->var
  (lambda (exp) exp))

;; lambda-exp : Var * Lc-exp -> Lc-exp
(define lambda-exp
  (lambda (bound-var body)
    (list 'lambda (list bound-var) body)))

;; lambda-exp? : Lc-exp -> Bool
(define lambda-exp?
  (lambda (exp)
    (and (pair? exp)
         (eqv? (car exp) 'lambda))))

;; lambda-exp->bound-var : Lc-exp -> Var
(define lambda-exp->bound-var
  (lambda (exp) (caadr exp)))

;; lambda-exp->body : Lc-exp -> Lc-exp
(define lambda-exp->body
  (lambda (exp) (caddr exp)))

;; app-exp : Lc-exp * Lc-exp -> Lc-exp
(define app-exp
  (lambda (rator rand)
    (list rator rand)))

;; app-exp? : Lc-exp -> Bool
(define app-exp?
  (lambda (exp)
    (and (list? exp)
         (= (length exp) 2))))

;; app-exp->rator : Lc-exp -> Lc-exp
(define app-exp->rator
  (lambda (exp) (car exp)))

;; app-exp->rand : Lc-exp -> Lc-exp
(define app-exp->rand
  (lambda (exp) (cadr exp)))
