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
    (cons (cons (list var) (list val))
          env)))

;; extend-env* : Listof(Var) * Listof(SchemeVal) * Env -> Env
;; usage: (extend-env* (var1 ... vark) (val1 ... valk) [f]) = [g]
;;        where g(var) = vali    if var = vari for some i such that 1 <= i <= k
;;                     | f(var)  otherwise
(define extend-env*
  (lambda (var-list val-list env)
    (cons (cons var-list val-list)
          env)))

;; search-var-val-list : Listof(Var) * Listof(SchemeVal) * Var -> SchemeVal
;; usage: (search-var-val-list (var1 ... vark) (val1 ... valk) var) = val-list
;;        if var = vari for some i such that 1 <= i <= k:
;;          (car val-list) = val
;;        else:
;;          (null? val-list) = #t
(define search-var-val-list
  (lambda (var-list val-list s)
    (define search-var-val-list4
      (lambda (var-list val-list s res)
        (if (null? var-list)
            res
            (if (eqv? s (car var-list))
                (search-var-val-list4 (cdr var-list) (cdr val-list) s
                                      (cons (car val-list) res))
                (search-var-val-list4 (cdr var-list) (cdr val-list) s
                                      res)))))
    (search-var-val-list4 var-list val-list s '())))

;; apply-env : Env * Var -> SchemeVal
;; usage: (apply-env [f] var) = f(var)
(define apply-env
  (lambda (env search-var)
    (cond
      ((null? env)
       (report-no-binding-found search-var))
      ((pair? (car env))
       (let ((saved-var-list (caar env))
             (saved-val-list (cdar env))
             (saved-env (cdr env)))
         (let ((search-val-list (search-var-val-list saved-var-list
                                                     saved-val-list
                                                     search-var)))
           (if (null? search-val-list)
               (apply-env (cdr env) search-var)
               (car search-val-list)))))
      (else
       (report-invalid-env env)))))

(define report-no-binding-found
  (lambda (search-var)
    (eopl:error 'apply-env "No binding for ~s" search-var)))

(define report-invalid-env
  (lambda (env)
    (eopl:error 'apply-env "Bad environment: ~s" env)))
