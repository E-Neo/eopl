#lang eopl

;; Env = Var -> (Var -> SchemeVal () -> Bool)

;; empty-env : () -> Env
(define empty-env
  (lambda ()
    (list (lambda (search-var)
            (report-no-binding-found search-var))
          (lambda () #t))))

;; extend-env : Var * SchemeVal * Env -> Env
(define extend-env
  (lambda (saved-var saved-val saved-env)
    (list (lambda (search-var)
            (if (eqv? search-var saved-var)
                saved-val
                (apply-env saved-env search-var)))
          (lambda () #f))))

;; apply-env : Env * Var -> SchemeVal
(define apply-env
  (lambda (env search-var)
    ((car env) search-var)))

;; empty-env? : Env -> Bool
(define empty-env?
  (lambda (env)
    ((cadr env))))

(define report-no-binding-found
  (lambda (search-var)
    (eopl:error 'apply-env "No binding for ~s" search-var)))

(define report-invalid-env
  (lambda (env)
    (eopl:error 'apply-env "Bad environment: ~s" env)))
