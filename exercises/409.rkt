#lang eopl

;;; Store:

;; empty-store : () -> Sto
(define empty-store
  (lambda () (make-vector 0)))

;; usage: A Scheme variable containing the current state of the store.
;;        Initially set to a dummy value.
(define the-store 'uninitialized)

;; get-store : () -> Sto
(define get-store
  (lambda () the-store))

;; initialize-store! : () -> Unspecified
;; usage: (initialize-store!) sets the-store to the empty store
(define initialize-store!
  (lambda ()
    (set! the-store (empty-store))))

;; reference? : SchemeVal -> Bool
(define reference?
  (lambda (x) (integer? x)))

;; newref : ExpVal -> Ref
;; We can use a better algorithm to get constant amortized time performance
;; rather than linear time.
(define newref
  (lambda (val)
    (let ((old-store the-store)
          (next-ref (vector-length the-store)))
      (set! the-store (make-vector (+ next-ref 1)))
      (let copy ((i 0))
        (if (= i next-ref)
            (vector-set! the-store i val)
            (begin (vector-set! the-store i (vector-ref old-store i))
                   (copy (+ i 1)))))
      next-ref)))

;; deref : Ref -> ExpVal
(define deref
  (lambda (ref)
    (vector-ref the-store ref)))

;; setref! : Ref * ExpVal -> Unspecified
;; usage: Sets the-store to a state like the original, but with position ref
;;        containing val.
(define setref!
  (lambda (ref val)
    (vector-set! the-store ref val)))
