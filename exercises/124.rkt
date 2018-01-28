#lang eopl

;; every? : Pred * List -> Bool
;; usage: (every? pred lst) returns #f if any element of lst fails to satisfy
;;        pred, and returns #t otherwise.
(define every?
  (lambda (pred lst)
    (if (null? lst)
        #t
        (if (pred (car lst))
            (every? pred (cdr lst))
            #f))))
