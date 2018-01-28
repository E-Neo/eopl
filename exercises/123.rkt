#lang eopl

;; list-index-count : Pred * List * Int -> Int/Bool
;; usage: (list-index-count pred lst count) returns count add the 0-based
;;        position of the first element of lst that satisfies the predicate
;;        pred. If no element of lst satisfies the predicate, then list-index
;;        returns #f.
(define list-index-count
  (lambda (pred lst count)
    (if (null? lst)
        #f
        (if (pred (car lst))
            count
            (list-index-count pred (cdr lst) (+ 1 count))))))

;; list-index : Pred * List -> Int/Bool
;; usage: (list-index pred lst) returns the 0-based position of the first
;;        element of lst that satisfies the predicate pred. If no element
;;        of lst satisfies the predicate, then list-index returns #f.
(define list-index
  (lambda (pred lst)
    (list-index-count pred lst 0)))
