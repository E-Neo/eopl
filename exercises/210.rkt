#lang eopl

;; extend-env* : Listof(Var) * Listof(SchemeVal) * Env -> Env
;; usage: (extend-env* (var1 ... vark) (val1 ... valk) [f]) = [g]
;;        where g(var) = vali    if var = vari for some i such that 1 <= i <= k
;;                     | f(var)  otherwise
(define extend-env*
  (lambda (var-list val-list env)
    (if (null? var-list)
        env
        (let ((var (car var-list))
              (val (car val-list)))
          (extend-env* (cdr var-list) (cdr val-list)
                       (cons (cons var val) env))))))
