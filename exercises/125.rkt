#lang eopl

;; exists? : Pred * List -> Bool
;; usage: (exists? pred lst) returns #t if any element of lst satisfies pred,
;;        and returns #f otherwise.
(define exists?
  (lambda (pred lst)
    (if (null? lst)
        #f
        (if (pred (car lst))
            #t
            (exists? pred (cdr lst))))))
