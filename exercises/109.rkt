#lang eopl

;; remove : Sym * Listof(Sym) -> Listof(Sym)
;; usage: (remove s los) returns a list with the same elements arranged in the
;;        same order as los, except that all the occurrence of the symbol s is
;;        removed.
(define remove
  (lambda (s los)
    (if (null? los)
        '()
        (if (eqv? s (car los))
            (remove s (cdr los))
            (cons (car los) (remove s (cdr los)))))))
