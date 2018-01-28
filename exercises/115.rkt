#lang eopl

;; duple : Int * SchemeVal -> Listof(SchemeVal)
;; usage: (duple n x) returns a list containing n copies of x.
(define duple
  (lambda (n x)
    (if (= 0 n)
        '()
        (cons x (duple (- n 1) x)))))
