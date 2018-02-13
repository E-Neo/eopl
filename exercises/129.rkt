#lang eopl

;; sort : Listof(Int) -> Listof(Int)
;; usage: (sort loi) returns a list of the elements of loi in ascending order.
(define sort
  (lambda (loi)
    (if (< (length loi) 2)
        loi
        (let ((mid (quotient (length loi) 2)))
          (merge (sort (list-head loi mid))
                 (sort (list-tail loi mid)))))))

;; list-head : Listof(Int) -> Listof(Int)
(define list-head
  (lambda (loi n)
    (if (zero? n)
        '()
        (append (list (car loi))
                (list-head (cdr loi) (- n 1))))))

;; merge : Listof(Int) * Listof(Int) -> Listof(Int)
;; usage: (merge loi1 loi2), where loi1 and loi2 are lists of integers that are
;;        sorted in ascending order, returns a sorted list of all the integers
;;        in loi1 and loi2.
(define merge
  (lambda (loi1 loi2)
    (cond
      ((null? loi1) loi2)
      ((null? loi2) loi1)
      ((< (car loi2) (car loi1))
       (cons (car loi2) (merge loi1 (cdr loi2))))
      (else
       (cons (car loi1) (merge (cdr loi1) loi2))))))
