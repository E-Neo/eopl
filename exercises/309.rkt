#lang eopl

;;; Examples:

;; let x = 4
;; in cons(x,
;;         cons(cons(-(x, 1),
;;                   emptylist),
;;              emptylist))


;;; Grammatical specification:

(define the-lexical-spec
  '((whitespace (whitespace) skip)
    (comment ("%" (arbno (not #\newline))) skip)
    (identifier
     (letter (arbno (or letter digit "_" "-" "?")))
     symbol)
    (number (digit (arbno digit)) number)
    (number ("-" digit (arbno digit)) number)))

(define the-grammar
  '((program (expression) a-program)
    (expression (number) const-exp)
    (expression
     ("-" "(" expression "," expression ")")
     diff-exp)
    (expression
     ("zero?" "(" expression ")")
     zero?-exp)
    (expression
     ("if" expression "then" expression "else" expression)
     if-exp)
    (expression (identifier) var-exp)
    (expression
     ("let" identifier "=" expression "in" expression)
     let-exp)
    (expression
     ("cons" "(" expression "," expression ")")
     cons-exp)
    (expression
     ("car" "(" expression ")")
     car-exp)
    (expression
     ("cdr" "(" expression ")")
     cdr-exp)
    (expression
     ("null?" "(" expression ")")
     null?-exp)))


;;; Syntax data types:

(define-datatype program program?
  (a-program
   (exp1 expression?)))

(define-datatype expression expression?
  (const-exp
   (num number?))
  (diff-exp
   (exp1 expression?)
   (exp2 expression?))
  (zero?-exp
   (exp1 expression?))
  (if-exp
   (exp1 expression?)
   (exp2 expression?)
   (exp3 expression?))
  (var-exp
   (var identifier?))
  (let-exp
   (var identifier?)
   (exp1 expression?)
   (body expression?))
  (cons-exp
   (exp1 expression?)
   (exp2 expression?))
  (car-exp
   (exp1 expression?))
  (cdr-exp
   (exp1 expression?))
  (null?-exp
   (exp1 expression?)))

(define identifier?
  (lambda (x)
    (symbol? x)))


;;; Expressed values:

(define-datatype expval expval?
  (num-val
   (num number?))
  (bool-val
   (bool boolean?))
  (pair-val
   (pair pair?)))

;; expval->num : ExpVal -> Int
(define expval->num
  (lambda (val)
    (cases expval val
           (num-val (num) num)
           (else (report-expval-extractor-error 'num val)))))

;; expval->bool : ExpVal -> Bool
(define expval->bool
  (lambda (val)
    (cases expval val
           (bool-val (bool) bool)
           (else (report-expval-extractor-error 'bool val)))))

;; expval->list : ExpVal -> Listof(ExpVal)
(define expval->pair
  (lambda (val)
    (cases expval val
           (pair-val (lst) lst)
           (else (report-expval-extractor-error 'list val)))))

(define report-expval-extractor-error
  (lambda (s val)
    (eopl:error s "ExpVal extractor error: ~s" val)))


;;; Interpreter:

;; run : String -> ExpVal
(define run
  (lambda (s)
    (value-of-program (scan&parse s))))

;; value-of-program : Program -> ExpVal
(define value-of-program
  (lambda (pgm)
    (cases program pgm
           (a-program (exp1)
                      (value-of exp1 (init-env))))))

;; value-of : Exp * Env -> ExpVal
(define value-of
  (lambda (exp env)
    (cases expression exp
           (const-exp (num) (num-val num))
           (diff-exp (exp1 exp2)
                     (let ((val1 (value-of exp1 env))
                           (val2 (value-of exp2 env)))
                       (let ((num1 (expval->num val1))
                             (num2 (expval->num val2)))
                         (num-val (- num1 num2)))))
           (zero?-exp (exp1)
                      (let ((val1 (value-of exp1 env)))
                        (let ((num1 (expval->num val1)))
                          (if (zero? num1)
                              (bool-val #t)
                              (bool-val #f)))))
           (if-exp (exp1 exp2 exp3)
                   (let ((val1 (value-of exp1 env)))
                     (if (expval->bool val1)
                         (value-of exp2 env)
                         (value-of exp3 env))))
           (var-exp (var) (apply-env env var))
           (let-exp (var exp1 body)
                    (let ((val1 (value-of exp1 env)))
                      (value-of body
                                (extend-env var val1 env))))
           (cons-exp (exp1 exp2)
                     (let ((val1 (value-of exp1 env))
                           (val2 (value-of exp2 env)))
                       (pair-val (cons val1 val2))))
           (car-exp (exp1)
                    (let ((val1 (value-of exp1 env)))
                      (car (expval->pair val1))))
           (cdr-exp (exp1)
                    (let ((val1 (value-of exp1 env)))
                      (cdr (expval->pair val1))))
           (null?-exp (exp1)
                      (let ((val1 (value-of exp1 env)))
                        (null? (expval->pair val1)))))))

;; init-env : () -> Env
(define init-env
  (lambda ()
    (extend-env 'emptylist '()
                (empty-env))))


;;; Environment:

;; Env = (empty-env) | (extend-env Var SchemeVal Env)

(define-datatype env env?
  (empty-env)
  (extend-env
   (saved-var symbol?)
   (saved-val (lambda (x) #t))
   (saved-env env?)))

;; apply-env : Env * Var -> SchemeVal
(define apply-env
  (lambda (e search-var)
    (cases env e
           (empty-env ()
                      (report-no-binding-found search-var))
           (extend-env (saved-var saved-val saved-env)
                       (if (eqv? search-var saved-var)
                           saved-val
                           (apply-env saved-env search-var))))))

(define report-no-binding-found
  (lambda (search-var)
    (eopl:error 'apply-env "No binding for ~s" search-var)))


;;; Scanner and Parser:

(define scan&parse
  (sllgen:make-string-parser the-lexical-spec the-grammar))
