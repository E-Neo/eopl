#lang eopl

;; up : List -> List
;; usage: (up lst) removes a pair of parentheses from each top-level element
;;        of lst. If a top-level element is not a list, it is included in the
;;        result, as is.
(define up
  (lambda (lst)
    (if (null? lst)
        '()
        (if (list? (car lst))
            (append (car lst) (up (cdr lst)))
            (cons (car lst) (up (cdr lst)))))))
