#lang eopl

;; swapper : Sym * Sym * S-List -> S-List
;; usage: (swapper s1 s2 slist) returns a list the same as slist, but with all
;;        occurrences of s1 replaced by s2 and all occurrences of s2 replaced
;;        by s1.
(define swapper
  (lambda (s1 s2 slist)
    (cond ((null? slist) '())
          ((eqv? s1 (car slist)) (cons s2 (swapper s1 s2 (cdr slist))))
          ((eqv? s2 (car slist)) (cons s1 (swapper s1 s2 (cdr slist))))
          (else (cons (car slist) (swapper s1 s2 (cdr slist)))))))
