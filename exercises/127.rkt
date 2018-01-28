#lang eopl

;; flatten : S-List -> Listof(Sym)
;; usage: (flatten slist) returns a list of the symbols contained in slist in
;;        the order in which they occur when slist is printed. Intuitively,
;;        flatten removes all the inner parentheses from its argument.
(define flatten
  (lambda (slist)
    (if (null? slist)
        '()
        (if (symbol? (car slist))
            (cons (car slist) (flatten (cdr slist)))
            (append (flatten (car slist)) (flatten (cdr slist)))))))
