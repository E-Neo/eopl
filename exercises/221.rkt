#lang eopl

;; Env = (empty-env) | (extend-env Var SchemeVal Env)

(define-datatype env env?
  (empty-env)
  (extend-env
   (saved-var symbol?)
   (saved-val (lambda (x) #t))
   (saved-env env?)))

;; apply-env : Env * Var -> SchemeVal
(define apply-env
  (lambda (e search-var)
    (cases env e
           (empty-env ()
                      (report-no-binding-found search-var))
           (extend-env (saved-var saved-val saved-env)
                       (if (eqv? search-var saved-var)
                           saved-val
                           (apply-env saved-env search-var))))))

;; has-binding? : Env * Var -> Bool
(define has-binding?
  (lambda (e search-var)
    (cases env e
           (empty-env () #f)
           (extend-env (saved-var saved-val saved-env)
                       (if (eqv? search-var saved-var)
                           #t
                           (has-binding? saved-env search-var))))))

(define report-no-binding-found
  (lambda (search-var)
    (eopl:error 'apply-env "No binding for ~s" search-var)))
