#lang eopl

(define-datatype stack stack?
  (empty-stack)
  (push
   (original-stack stack?)
   (x (lambda (x) #t))))

;; pop : Stack -> Stack
(define pop
  (lambda (s)
    (cases stack s
           (empty-stack () (report-empty-stack 'pop s))
           (push (original-stack x) original-stack))))

;; top : Stack -> SchemeVal
(define top
  (lambda (s)
    (cases stack s
           (empty-stack () (report-empty-stack 'top s))
           (push (original-stack x) x))))

;; empty-stack? : Stack -> Bool
(define empty-stack?
  (lambda (s)
    (cases stack s
           (empty-stack () #t)
           (push (original-stack x) #f))))

(define report-empty-stack
  (lambda (m s)
    (eopl:error m "Empty stack: ~s" s)))
