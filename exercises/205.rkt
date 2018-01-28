#lang eopl

;; Env = (empty-env) | (extend-env Var SchemeVal Env)
;; Var = Sym

;; empty-env : () -> Env
;; usage: (empty-env) = [empty-environment]
(define empty-env
  (lambda () '()))

;; extend-env : Var * SchemeVal * Env -> Env
;; usage: (extend-env var v [f]) = [g]
;;        where g(var1) = v        if var1 = var
;;                      | f(var1)  otherwise
(define extend-env
  (lambda (var val env)
    (cons (cons var val)
          env)))

;; apply-env : Env * Var -> SchemeVal
;; usage: (apply-env [f] var) = f(var)
(define apply-env
  (lambda (env search-var)
    (cond
      ((null? env)
       (report-no-binding-found search-var))
      ((pair? (car env))
       (let ((saved-var (caar env))
             (saved-val (cdar env))
             (saved-env (cdr env)))
         (if (eqv? search-var saved-var)
             saved-val
             (apply-env saved-env search-var))))
      (else
       (report-invalid-env env)))))

(define report-no-binding-found
  (lambda (search-var)
    (eopl:error 'apply-env "No binding for ~s" search-var)))

(define report-invalid-env
  (lambda (env)
    (eopl:error 'apply-env "Bad environment: ~s" env)))
