#lang eopl

;; nth-element : List * Int -> SchemeVal
;; usage: (nth-element lst n) = the n-th element of lst
(define nth-element
  (lambda (lst n)
    (nth-element4 lst n lst n)))

;; nth-element4 : List * Int * List * Int -> SchemeVal
;; usage: (nth-element lst n
;;                     original-lst original-n) = the n-th element of lst
(define nth-element4
  (lambda (lst n original-lst original-n)
    (if (null? lst)
        (report-list-too-short original-lst original-n)
        (if (zero? n)
            (car lst)
            (nth-element4 (cdr lst) (- n 1) original-lst original-n)))))

(define report-list-too-short
  (lambda (lst n)
    (eopl:error 'nth-element "~s does not have ~s elements" lst n)))
