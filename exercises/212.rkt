#lang eopl

;; Stack = Sym -> SchemeVal

;; empty-stack : () -> Stack
(define empty-stack
  (lambda ()
    (lambda (m)
      (cond
        ((eqv? m 'top) (report-invalid-method m))
        ((eqv? m 'empty-stack?) #t)
        ((eqv? m 'pop) (report-empty-stack m))
        (else (report-invalid-method m))))))

;; push : Stack * SchemeVal -> Stack
(define push
  (lambda (s x)
    (lambda (m)
      (cond
        ((eqv? m 'top) x)
        ((eqv? m 'empty-stack?) #f)
        ((eqv? m 'pop) s)
        (else (report-invalid-method m))))))

;; pop : Stack -> Stack
(define pop
  (lambda (s)
    (if (empty-stack? s)
        (report-empty-stack 'pop)
        (s 'pop))))

;; top : Stack -> SchemeVal
(define top
  (lambda (s)
    (s 'top)))

;; empty-stack? : Stack -> Bool
(define empty-stack?
  (lambda (s)
    (s 'empty-stack?)))

(define report-empty-stack
  (lambda (m)
    (eopl:error m "Empty stack")))

(define report-invalid-method
  (lambda (m)
    (eopl:error m "Invalid method")))
