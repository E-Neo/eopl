#lang eopl

;; empty-env? : Env -> Bool
;; usage: (empty-env? [f]) = #t  if f is empty environment
;;                         | #f  otherwise
(define empty-env?
  (lambda (env)
    (null? env)))
