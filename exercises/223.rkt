#lang eopl

(define identifier?
  (lambda (x)
    (and (symbol? x)
         (not (eqv? x 'lambda)))))
