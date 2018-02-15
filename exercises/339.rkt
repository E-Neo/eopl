#lang eopl

;;; Examples:

;; % The value of the following program is 4
;; let u = 7
;; in unpack x y = cons(u, cons(3, emptylist))
;;    in -(x, y)


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
    (expression ("%lexref" number) nameless-var-exp)
    (expression ("%let" expression "in" expression) nameless-let-exp)
    (expression ("%lexproc" expression) nameless-proc-exp)
    (expression ("%unpack" expression "in" expression) nameless-unpack-exp)
    (expression
     ("cons" "(" expression "," expression ")")
     cons-exp)
    (expression ("emptylist") emptylist-exp)
    (expression
     ("unpack" (arbno identifier) "=" expression "in" expression)
     unpack-exp)))


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
  (nameless-var-exp
   (n number?))
  (nameless-let-exp
   (exp1 expression?)
   (body expression?))
  (nameless-proc-exp
   (body expression?))
  (cons-exp
   (exp1 expression?)
   (exp2 expression?))
  (emptylist-exp)
  (unpack-exp
   (vars (list-of identifier?))
   (exp1 expression?)
   (body expression?))
  (nameless-unpack-exp
   (exp1 expression?)
   (body expression?)))

(define identifier?
  (lambda (x)
    (symbol? x)))


;;; Translator:

;; translation-of-program : Program -> Nameless-program
(define translation-of-program
  (lambda (pgm)
    (cases program pgm
           (a-program (exp1)
                      (a-program (translation-of exp1 (init-senv)))))))

;; translation-of : Exp * Senv -> Nameless-exp
(define translation-of
  (lambda (exp senv)
    (cases expression exp
           (const-exp (num) (const-exp num))
           (diff-exp (exp1 exp2)
                     (diff-exp (translation-of exp1 senv)
                               (translation-of exp2 senv)))
           (zero?-exp (exp1)
                      (zero?-exp (translation-of exp1 senv)))
           (if-exp (exp1 exp2 exp3)
                   (if-exp (translation-of exp1 senv)
                           (translation-of exp2 senv)
                           (translation-of exp3 senv)))
           (var-exp (var)
                    (nameless-var-exp (apply-senv senv var)))
           (let-exp (var exp1 body)
                    (nameless-let-exp
                     (translation-of exp1 senv)
                     (translation-of body
                                     (extend-senv var senv))))
           (proc-exp (var body)
                     (nameless-proc-exp
                      (translation-of body
                                      (extend-senv var senv))))
           (call-exp (rator rand)
                     (call-exp (translation-of rator senv)
                               (translation-of rand senv)))
           (cons-exp (exp1 exp2)
                     (cons-exp (translation-of exp1 senv)
                               (translation-of exp2 senv)))
           (emptylist-exp () (emptylist-exp))
           (unpack-exp (vars exp1 body)
                       (nameless-unpack-exp
                        (translation-of exp1 senv)
                        (translation-of body
                                        (extend-senv* vars senv))))
           (else (report-invalid-source-expression exp)))))

(define report-invalid-source-expression
  (lambda (exp)
    (eopl:error 'translation-of
                "Illegal expression in source code: ~s" exp)))

;; init-senv : () -> Senv
(define init-senv
  (lambda () (empty-senv)))


;;; Static environment:

;; Senv = Listof(Var)
;; Lexaddr = N

;; empty-senv : () -> Senv
(define empty-senv
  (lambda () '()))

;; extend-senv : Var * Senv -> Senv
(define extend-senv
  (lambda (var senv)
    (cons var senv)))

;; extend-senv* : Listof(Vars) * Senv -> Senv
(define extend-senv*
  (lambda (vars senv)
    (if (null? vars)
        senv
        (extend-senv* (cdr vars)
                      (extend-senv (car vars) senv)))))

;; apply-senv : Senv * Var -> Lexaddr
(define apply-senv
  (lambda (senv var)
    (cond
      ((null? senv) (report-unbound-var var))
      ((eqv? var (car senv)) 0)
      (else (+ 1 (apply-senv (cdr senv) var))))))

(define report-unbound-var
  (lambda (var)
    (eopl:error 'translation-of "unbound variable in code: ~s" var)))


;;; Expressed values:

(define-datatype expval expval?
  (num-val
   (num number?))
  (bool-val
   (bool boolean?))
  (proc-val
   (proc proc?))
  (list-val
   (lst (list-of expval?))))

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

;; expval->list : ExpVal -> Listof(ExpVal)
(define expval->list
  (lambda (val)
    (cases expval val
           (list-val (lst) lst)
           (else (report-expval-extractor-error 'list val)))))

(define report-expval-extractor-error
  (lambda (s val)
    (eopl:error s "ExpVal extractor error: ~s" val)))

;; proc? : SchemeVal -> Bool
;; procedure : Nameless-exp * Nameless-env -> Proc
(define-datatype proc proc?
  (procedure
   (body expression?)
   (saved-nameless-env nameless-environment?)))

;; apply-procedure : Proc * ExpVal -> ExpVal
(define apply-procedure
  (lambda (proc1 val)
    (cases proc proc1
           (procedure (body saved-nameless-env)
                      (value-of
                       body
                       (extend-nameless-env val saved-nameless-env))))))


;;; Interpreter:

;; run : String -> ExpVal
(define run
  (lambda (s)
    (value-of-program (translation-of-program (scan&parse s)))))

;; value-of-program : Program -> ExpVal
(define value-of-program
  (lambda (pgm)
    (cases program pgm
           (a-program (exp1)
                      (value-of exp1 (init-nameless-env))))))

;; value-of : Exp * Nameless-env -> ExpVal
(define value-of
  (lambda (exp nameless-env)
    (cases expression exp
           (const-exp (num) (num-val num))
           (diff-exp (exp1 exp2)
                     (let ((val1 (value-of exp1 nameless-env))
                           (val2 (value-of exp2 nameless-env)))
                       (let ((num1 (expval->num val1))
                             (num2 (expval->num val2)))
                         (num-val (- num1 num2)))))
           (zero?-exp (exp1)
                      (let ((val1 (value-of exp1 nameless-env)))
                        (let ((num1 (expval->num val1)))
                          (if (zero? num1)
                              (bool-val #t)
                              (bool-val #f)))))
           (if-exp (exp1 exp2 exp3)
                   (let ((val1 (value-of exp1 nameless-env)))
                     (if (expval->bool val1)
                         (value-of exp2 nameless-env)
                         (value-of exp3 nameless-env))))
           (nameless-var-exp (n) (apply-nameless-env nameless-env n))
           (nameless-let-exp (exp1 body)
                             (let ((val1 (value-of exp1 nameless-env)))
                               (value-of body
                                         (extend-nameless-env val1
                                                              nameless-env))))
           (nameless-proc-exp (body)
                              (proc-val (procedure body nameless-env)))
           (call-exp (rator rand)
                     (let ((proc (expval->proc (value-of rator nameless-env)))
                           (arg (value-of rand nameless-env)))
                       (apply-procedure proc arg)))
           (cons-exp (exp1 exp2)
                     (let ((val1 (value-of exp1 nameless-env))
                           (lst (expval->list (value-of exp2 nameless-env))))
                       (list-val (cons val1 lst))))
           (emptylist-exp () (list-val '()))
           (nameless-unpack-exp
            (exp1 body)
            (let ((lst (expval->list
                        (value-of exp1 nameless-env))))
              (value-of body
                        (extend-nameless-env* lst
                                              nameless-env))))
           (else (report-invalid-translated-expression exp)))))

(define report-invalid-translated-expression
  (lambda (exp)
    (eopl:error 'value-of
                "Illegal expression in translated code: ~s" exp)))

;; init-nameless-env : () -> Nameless-env
(define init-nameless-env
  (lambda ()
    (empty-nameless-env)))


;;; Nameless environment:

;; nameless-environment? : SchemeVal -> Bool
(define nameless-environment?
  (lambda (x)
    ((list-of expval?) x)))

;; empty-nameless-env : () -> Nameless-env
(define empty-nameless-env
  (lambda () '()))

;; extend-nameless-env : ExpVal * Nameless-env -> Nameless-env
(define extend-nameless-env
  (lambda (val nameless-env)
    (cons val nameless-env)))

;; extend-nameless-env* : Listof(ExpVal) * Nameless-env -> Nameless-env
(define extend-nameless-env*
  (lambda (vals nameless-env)
    (if (null? vals)
        nameless-env
        (extend-nameless-env* (cdr vals)
                              (extend-nameless-env (car vals)
                                                   nameless-env)))))

;; apply-nameless-env : Nameless-env * Lexaddr -> ExpVal
(define apply-nameless-env
  (lambda (nameless-env n)
    (list-ref nameless-env n)))


;;; Scanner and Parser:

(define scan&parse
  (sllgen:make-string-parser the-lexical-spec the-grammar))
