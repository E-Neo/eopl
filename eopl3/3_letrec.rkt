#lang eopl

;;; Examples:

;; % The value of the following program is 12
;; letrec double(x)
;;        = if zero?(x) then 0 else -((double -(x, 1)), -2)
;; in (double 6)


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
     ("proc" "(" identifier ")" expression)
     proc-exp)
    (expression
     ("(" expression expression ")")
     call-exp)
    (expression
     ("letrec" identifier "(" identifier ")" "=" expression "in" expression)
     letrec-exp)))


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
  (proc-exp
   (var identifier?)
   (body expression?))
  (call-exp
   (rator expression?)
   (rand expression?))
  (letrec-exp
   (proc-name identifier?)
   (bound-var identifier?)
   (proc-body expression?)
   (letrec-body expression?)))

(define identifier?
  (lambda (x)
    (symbol? x)))

;; proc? : SchemeVal -> Bool
;; procedure : Var * Exp * Env -> Proc
(define-datatype proc proc?
  (procedure
   (var identifier?)
   (body expression?)
   (saved-env environment?)))

;; apply-procedure : Proc * ExpVal -> ExpVal
(define apply-procedure
  (lambda (proc1 val)
    (cases proc proc1
           (procedure (var body saved-env)
                      (value-of body (extend-env var val saved-env))))))


;;; Expressed values:

(define-datatype expval expval?
  (num-val
   (num number?))
  (bool-val
   (bool boolean?))
  (proc-val
   (proc proc?)))

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

;; expval->proc : ExpVal -> Proc
(define expval->proc
  (lambda (val)
    (cases expval val
           (proc-val (proc) proc)
           (else (report-expval-extractor-error 'proc val)))))

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
           (proc-exp (var body)
                     (proc-val (procedure var body env)))
           (call-exp (rator rand)
                     (let ((proc (expval->proc (value-of rator env)))
                           (arg (value-of rand env)))
                       (apply-procedure proc arg)))
           (letrec-exp (proc-name bound-var proc-body letrec-body)
                       (value-of letrec-body
                                 (extend-env-rec proc-name bound-var proc-body env))))))

;; init-env : () -> Env
(define init-env
  (lambda ()
    (empty-env)))


;;; Environment:

;; Env = (empty-env) | (extend-env Var ExpVal Env)

(define-datatype environment environment?
  (empty-env)
  (extend-env
   (saved-var symbol?)
   (saved-val expval?)
   (saved-env environment?))
  (extend-env-rec
   (p-name identifier?)
   (b-var identifier?)
   (body expression?)
   (saved-env environment?)))

;; apply-env : Env * Var -> ExpVal
(define apply-env
  (lambda (e search-var)
    (cases environment e
           (empty-env ()
                      (report-no-binding-found search-var))
           (extend-env (saved-var saved-val saved-env)
                       (if (eqv? search-var saved-var)
                           saved-val
                           (apply-env saved-env search-var)))
           (extend-env-rec (p-name b-var body saved-env)
                           (if (eqv? search-var p-name)
                               (proc-val (procedure b-var body e))
                               (apply-env saved-env search-var))))))

(define report-no-binding-found
  (lambda (search-var)
    (eopl:error 'apply-env "No binding for ~s" search-var)))


;;; Scanner and Parser:

(define scan&parse
  (sllgen:make-string-parser the-lexical-spec the-grammar))
