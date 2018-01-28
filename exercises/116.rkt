#lang eopl

;; invert : Listof(2-list) -> Listof(2-list)
;; usage: (invert lst), where lst is a list of 2-lists (lists of length two),
;;        returns a list with each 2-list reversed.
(define invert
  (lambda (lst)
    (if (null? lst)
        '()
        (cons (list (cadar lst)
                    (caar lst))
              (invert (cdr lst))))))
