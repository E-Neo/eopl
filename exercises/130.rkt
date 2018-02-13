#lang eopl

;; sort/predicate : Pred * Listof(Int) -> Listof(Int)
(define sort/predicate
  (lambda (pred loi)
    (if (< (length loi) 2)
        loi
        (let ((mid (quotient (length loi) 2)))
          (merge/predicate pred
                           (sort/predicate pred (list-head loi mid))
                           (sort/predicate pred (list-tail loi mid)))))))

;; list-head : Listof(Int) -> Listof(Int)
(define list-head
  (lambda (loi n)
    (if (zero? n)
        '()
        (append (list (car loi))
                (list-head (cdr loi) (- n 1))))))

;; merge/predicate : Pred * Listof(Int) * Listof(Int) -> Listof(Int)
(define merge/predicate
  (lambda (pred loi1 loi2)
    (cond
      ((null? loi1) loi2)
      ((null? loi2) loi1)
      ((pred (car loi2) (car loi1))
       (cons (car loi2) (merge/predicate pred loi1 (cdr loi2))))
      (else
       (cons (car loi1) (merge/predicate pred (cdr loi1) loi2))))))
