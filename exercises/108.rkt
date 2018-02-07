#lang eopl

;; remove-first : Sym * Listof(Sym) -> Listof(Sym)
;; usage: (remove-first s los) returns a list with the same elements arranged
;;        in the same order as los, except that the first occurrence of the
;;        symbol s and the elements before it are removed.
(define remove-first
  (lambda (s los)
    (if (null? los)
        '()
        (if (eqv? (car los) s)
            (cdr los)
            (remove-first s (cdr los))))))
