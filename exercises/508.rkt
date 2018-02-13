#lang eopl

;;; Examples:

;; % The value of the following program is 10
;; let f = proc (x, y, z) -(x, -(0, -(y, -(0, z))))
;; in (f 2 3 5)


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
     ("proc" "(" (separated-list identifier ",") ")" expression)
     proc-exp)
    (expression
     ("(" expression (arbno expression) ")")
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
   (vars (list-of identifier?))
   (body expression?))
  (call-exp
   (rator expression?)
   (rands (list-of expression?)))
  (letrec-exp
   (proc-name identifier?)
   (bound-var identifier?)
   (proc-body expression?)
   (letrec-body expression?)))

(define identifier?
  (lambda (x)
    (symbol? x)))


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
;; procedure : Listof(Var) * Exp * Env -> Proc
(define-datatype proc proc?
  (procedure
   (vars (list-of identifier?))
   (body expression?)
   (saved-env environment?)))

;; apply-procedure/k : Proc * Listof(ExpVal) * Cont -> FinalAnswer
(define apply-procedure/k
  (lambda (proc1 vals cont)
    (cases proc proc1
           (procedure (vars body saved-env)
                      (value-of/k body
                                  (extend-env* vars vals saved-env)
                                  cont)))))


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
                      (value-of/k exp1 (init-env) (end-cont))))))

;; value-of/k : Exp * Env * Cont -> FinalAnswer
(define value-of/k
  (lambda (exp env cont)
    (cases expression exp
           (const-exp (num) (apply-cont cont (num-val num)))
           (diff-exp (exp1 exp2)
                     (value-of/k exp1 env
                                 (diff1-cont exp2 env cont)))
           (zero?-exp (exp1)
                      (value-of/k exp1 env (zero1-cont cont)))
           (if-exp (exp1 exp2 exp3)
                   (value-of/k exp1 env
                               (if-test-cont exp2 exp3 env cont)))
           (var-exp (var) (apply-cont cont (apply-env env var)))
           (let-exp (var exp1 body)
                    (value-of/k exp1 env
                                (let-exp-cont var body env cont)))
           (proc-exp (vars body)
                     (apply-cont cont (proc-val (procedure vars body env))))
           (call-exp (rator rands)
                     (value-of/k rator env
                                 (rator-cont rands env cont)))
           (letrec-exp (p-name b-var p-body letrec-body)
                       (value-of/k letrec-body
                                   (extend-env-rec p-name b-var p-body env)
                                   cont)))))

;; init-env : () -> Env
(define init-env
  (lambda ()
    (empty-env)))


;;; Environment:

;; Env = (empty-env)
;;     | (extend-env Var ExpVal Env)
;;     | (extend-env-rec Var Var Exp Env)

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

;; extend-env* : Listof(Var) * Listof(ExpVal) * Env -> Env
;; usage: (extend-env* (var1 ... vark) (val1 ... valk) [f]) = [g]
;;        where g(var) = vali    if var = vari for some i such that 1 <= i <= k
;;                     | f(var)  otherwise
(define extend-env*
  (lambda (var-list val-list env)
    (if (null? var-list)
        env
        (let ((var (car var-list))
              (val (car val-list)))
          (extend-env* (cdr var-list) (cdr val-list)
                       (extend-env var val env))))))

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


;;; Continuation:

(define-datatype continuation continuation?
  (end-cont)
  (zero1-cont
   (cont continuation?))
  (let-exp-cont
   (var identifier?)
   (body expression?)
   (env environment?)
   (cont continuation?))
  (if-test-cont
   (exp2 expression?)
   (exp3 expression?)
   (env environment?)
   (cont continuation?))
  (diff1-cont
   (exp2 expression?)
   (env environment?)
   (cont continuation?))
  (diff2-cont
   (val1 expval?)
   (cont continuation?))
  (rator-cont
   (rands (list-of expression?))
   (env environment?)
   (cont continuation?))
  (rands-cont
   (val1 expval?)
   (rands (list-of expression?))
   (prev-args (list-of expval?))
   (env environment?)
   (cont continuation?)))

;; apply-cont : Cont * ExpVal -> FinalAnswer
(define apply-cont
  (lambda (cont val)
    (cases continuation cont
           (end-cont ()
                     (begin
                       (eopl:printf "End of computation.~%")
                       val))
           (zero1-cont (saved-cont)
                       (apply-cont saved-cont
                                   (bool-val (zero? (expval->num val)))))
           (let-exp-cont (var body saved-env saved-cont)
                         (value-of/k body
                                     (extend-env var val saved-env)
                                     saved-cont))
           (if-test-cont (exp2 exp3 saved-env saved-cont)
                         (if (expval->bool val)
                             (value-of/k exp2 saved-env saved-cont)
                             (value-of/k exp3 saved-env saved-cont)))
           (diff1-cont (exp2 saved-env saved-cont)
                       (value-of/k exp2 saved-env
                                   (diff2-cont val saved-cont)))
           (diff2-cont (val1 saved-cont)
                       (let ((num1 (expval->num val1))
                             (num2 (expval->num val)))
                         (apply-cont saved-cont
                                     (num-val (- num1 num2)))))
           (rator-cont (rands saved-env saved-cont)
                       (if (null? rands)
                           (apply-procedure/k (expval->proc val) '()
                                              saved-cont)
                           (value-of/k (car rands) saved-env
                                       (rands-cont val (cdr rands) '()
                                                   saved-env saved-cont))))
           (rands-cont (val1 rands prev-args saved-env saved-cont)
                       (if (null? rands)
                           (apply-procedure/k (expval->proc val1)
                                              (append prev-args (list val))
                                              saved-cont)
                           (value-of/k (car rands) saved-env
                                       (rands-cont val1 (cdr rands)
                                                   (append prev-args
                                                           (list val))
                                                   saved-env saved-cont)))))))


;;; Scanner and Parser:

(define scan&parse
  (sllgen:make-string-parser the-lexical-spec the-grammar))
