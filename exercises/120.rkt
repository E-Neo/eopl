#lang eopl

;; count-occurrences : Sym * S-List -> Int
;; usage: (count-occurrences s slist) returns the number of occurrences of s
;;        in slist.
(define count-occurrences
  (lambda (s slist)
    (if (null? slist)
        0
        (if (symbol? (car slist))
            (if (eqv? s (car slist))
                (+ (count-occurrences s (cdr slist)) 1)
                (count-occurrences s (cdr slist)))
            (+ (count-occurrences s (car slist))
               (count-occurrences s (cdr slist)))))))
