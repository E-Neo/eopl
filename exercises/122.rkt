#lang eopl

;; filter-in : Pred * List -> List
;; usage: (filter-in pred lst) returns the list of those elements in lst
;;        that satisfy the predicate pred.
(define filter-in
  (lambda (pred lst)
    (if (null? lst)
        '()
        (if (pred (car lst))
            (cons (car lst) (filter-in pred (cdr lst)))
            (filter-in pred (cdr lst))))))
