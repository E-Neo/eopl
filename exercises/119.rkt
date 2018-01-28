#lang eopl

;; list-set : Listof(SchemeVal) * Int * SchemeVal -> Listof(SchemeVal)
;; usage: (list-set lst n x) returns a list like lst, except that the n-th
;;        element, using zero-based indexing, is x.
(define list-set
  (lambda (lst n x)
    (if (null? lst)
        (report-list-too-short n)
        (if (zero? n)
            (cons x (cdr lst))
            (cons (car lst) (list-set (cdr lst) (- n 1) x))))))

(define report-list-too-short
  (lambda (n)
    (eopl:error 'list-set
                "List too short by ~s elements.~%" (+ n 1))))
