#lang eopl

;;; Examples:

;; % The value of the following program is -1
;; let x = 10
;;     y = 9
;;     f = proc (x, y) -(x, y)
;; in if zero?(x) then (f x y) else (f y x)


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
     ("let" (arbno identifier "=" expression) "in" expression)
     let-exp)
    (expression
     ("proc" "(" (separated-list identifier ",") ")" expression)
     proc-exp)
    (expression
     ("(" expression (arbno expression) ")")
     call-exp)))


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
   (vars (list-of identifier?))
   (exps (list-of expression?))
   (body expression?))
  (proc-exp
   (vars (list-of identifier?))
   (body expression?))
  (call-exp
   (rator expression?)
   (rands (list-of expression?)))
  (nameless-var-exp
   (n (pair-of number?)))
  (nameless-let-exp
   (exps (list-of expression?))
   (body expression?))
  (nameless-proc-exp
   (body expression?)))

(define pair-of
  (lambda (pred)
    (lambda (x)
      (and (pair? x)
           (pred (car x))
           (pred (cdr x))))))

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
           (let-exp (vars exps body)
                    (nameless-let-exp
                     (map (lambda (exp1)
                            (translation-of exp1 senv))
                          exps)
                     (translation-of body
                                     (extend-senv* vars senv))))
           (proc-exp (vars body)
                     (nameless-proc-exp
                      (translation-of body
                                      (extend-senv* vars senv))))
           (call-exp (rator rands)
                     (call-exp (translation-of rator senv)
                               (map (lambda (rand)
                                      (translation-of rand senv))
                                    rands)))
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
;; Lexaddr = (Int . Int)

;; empty-senv : () -> Senv
(define empty-senv
  (lambda () '()))

;; extend-senv* : Listof(Var) * Senv -> Senv
(define extend-senv*
  (lambda (vars senv)
    (cons vars senv)))

;; apply-senv : Senv * Var -> Lexaddr
(define apply-senv
  (lambda (senv var)
    (if (null? senv)
        (report-unbound-var var)
        (letrec
            ((index-of-inner
              (lambda (lst v idx)
                (if (null? lst)
                    #f
                    (if (eqv? (car lst) v)
                        idx
                        (index-of-inner (cdr lst) v (+ idx 1)))))))
          (let ((n2 (index-of-inner (car senv) var 0)))
            (if n2
                (cons 0 n2)
                ((lambda (x)
                   (cons (+ (car x) 1)
                         (cdr x)))
                 (apply-senv (cdr senv) var))))))))

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

;; proc? : SchemeVal -> Bool
;; procedure : Nameless-exp * Nameless-env -> Proc
(define-datatype proc proc?
  (procedure
   (body expression?)
   (saved-nameless-env nameless-environment?)))

;; apply-procedure : Proc * Listof(ExpVal) -> ExpVal
(define apply-procedure
  (lambda (proc1 vals)
    (cases proc proc1
           (procedure (body saved-nameless-env)
                      (value-of
                       body
                       (extend-nameless-env* vals saved-nameless-env))))))


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
           (nameless-var-exp (addr) (apply-nameless-env nameless-env addr))
           (nameless-let-exp (exps body)
                             (let ((vals (map (lambda (exp1)
                                                (value-of exp1 nameless-env))
                                              exps)))
                               (value-of body
                                         (extend-nameless-env* vals
                                                               nameless-env))))
           (nameless-proc-exp (body)
                              (proc-val (procedure body nameless-env)))
           (call-exp (rator rands)
                     (let ((proc (expval->proc (value-of rator nameless-env)))
                           (args (map (lambda (rand)
                                        (value-of rand nameless-env))
                                      rands)))
                       (apply-procedure proc args)))
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
    ((list-of (list-of expval?)) x)))

;; empty-nameless-env : () -> Nameless-env
(define empty-nameless-env
  (lambda () '()))

;; extend-nameless-env* : Listof(ExpVal) * Nameless-env -> Nameless-env
(define extend-nameless-env*
  (lambda (vals nameless-env)
    (cons vals nameless-env)))

;; apply-nameless-env : Nameless-env * Lexaddr -> ExpVal
(define apply-nameless-env
  (lambda (nameless-env addr)
    (list-ref (list-ref nameless-env (car addr))
              (cdr addr))))


;;; Scanner and Parser:

(define scan&parse
  (sllgen:make-string-parser the-lexical-spec the-grammar))
