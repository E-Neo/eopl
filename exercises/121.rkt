#lang eopl

;; product-symbol-list : Sym * Listof(Sym) -> Listof(2-list)
;; usage: (product-symbol-list s '(s0 s1 s2 ...))
;;        = ((s s0) (s s1) (s s2) ...)
(define product-symbol-list
  (lambda (s sos)
    (if (null? sos)
        '()
        (cons (list s (car sos))
              (product-symbol-list s (cdr sos))))))

;; product : Listof(Sym) * Listof(Sym) -> Listof(2-list)
;; usage: (product sos1 sos2), where sos1 and sos2 are each a list of symbols
;;        without repetitions, returns a list of 2-lists that represents the
;;        Cartesian product of sos1 and sos2. The 2-lists may appear in any
;;        order.
(define product
  (lambda (sos1 sos2)
    (if (null? sos1)
        '()
        (append (product-symbol-list (car sos1) sos2)
                (product (cdr sos1) sos2)))))
