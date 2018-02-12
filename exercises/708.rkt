#lang eopl

;;; Examples:

;; % The value of the following program is 1
;; let p = newpair(1, zero?(0))
;; in unpair x y = p
;;    in if y then 1 else 0


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
     ("proc" "(" identifier ":" type ")" expression)
     proc-exp)
    (expression
     ("(" expression expression ")")
     call-exp)
    (expression
     ("letrec"
      type identifier "(" identifier ":" type ")" "=" expression
      "in" expression)
     letrec-exp)
    (type ("int") int-type)
    (type ("bool") bool-type)
    (type ("(" type "->" type ")") proc-type)
    (type ("pairof" type "*" type) pair-type)
    (expression
     ("newpair" "(" expression "," expression ")")
     pair-exp)
    (expression
     ("unpair" identifier identifier "=" expression "in" expression)
     unpair-exp)))


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
   (var-type type?)
   (body expression?))
  (call-exp
   (rator expression?)
   (rand expression?))
  (letrec-exp
   (p-result-type type?)
   (p-name identifier?)
   (b-var identifier?)
   (b-var-type type?)
   (p-body expression?)
   (letrec-body expression?))
  (pair-exp
   (exp1 expression?)
   (exp2 expression?))
  (unpair-exp
   (var1 identifier?)
   (var2 identifier?)
   (exp expression?)
   (body expression?)))

(define identifier?
  (lambda (x)
    (symbol? x)))


;;; Types:

(define-datatype type type?
  (int-type)
  (bool-type)
  (proc-type
   (arg-type type?)
   (result-type type?))
  (pair-type
   (ty1 type?)
   (ty2 type?)))

(define type-to-external-form
  (lambda (ty)
    (cases type ty
           (int-type () 'int)
           (bool-type () 'bool)
           (proc-type (arg-type result-type)
                      (list (type-to-external-form arg-type)
                            '->
                            (type-to-external-form result-type)))
           (pair-type (ty1 ty2)
                      (list 'pairof
                            (type-to-external-form ty1)
                            '*
                            (type-to-external-form ty2))))))


;;; Type checker:

;; check : String -> external-type
(define check
  (lambda (s)
    (type-to-external-form
     (type-of-program (scan&parse s)))))

;; check-equal-type! : Type * Type * Exp -> Unspecified
(define check-equal-type!
  (lambda (ty1 ty2 exp)
    (when (not (equal? ty1 ty2))
      (report-unequal-types ty1 ty2 exp))))

(define report-unequal-types
  (lambda (ty1 ty2 exp)
    (eopl:error 'check-equal-type!
                "Types didn't match ~s != ~s in~%~s"
                (type-to-external-form ty1)
                (type-to-external-form ty2)
                exp)))

;; type-of-program : Program -> Type
(define type-of-program
  (lambda (pgm)
    (cases program pgm
           (a-program (exp1) (type-of exp1 (init-tenv))))))

;; type-of : Exp * TEnv -> Type
(define type-of
  (lambda (exp tenv)
    (cases expression exp
           (const-exp (num) (int-type))
           (diff-exp (exp1 exp2)
                     (let ((ty1 (type-of exp1 tenv))
                           (ty2 (type-of exp2 tenv)))
                       (check-equal-type! ty1 (int-type) exp1)
                       (check-equal-type! ty2 (int-type) exp2)
                       (int-type)))
           (zero?-exp (exp1)
                      (let ((ty1 (type-of exp1 tenv)))
                        (check-equal-type! ty1 (int-type) exp1)
                        (bool-type)))
           (if-exp (exp1 exp2 exp3)
                   (let ((ty1 (type-of exp1 tenv))
                         (ty2 (type-of exp2 tenv))
                         (ty3 (type-of exp3 tenv)))
                     (check-equal-type! ty1 (bool-type) exp1)
                     (check-equal-type! ty2 ty3 exp)
                     ty2))
           (var-exp (var) (apply-tenv tenv var))
           (let-exp (var exp1 body)
                    (let ((exp1-type (type-of exp1 tenv)))
                      (type-of body
                               (extend-tenv var exp1-type tenv))))
           (proc-exp (var var-type body)
                     (let ((result-type
                            (type-of body (extend-tenv var var-type tenv))))
                       (proc-type var-type result-type)))
           (call-exp (rator rand)
                     (let ((rator-type (type-of rator tenv))
                           (rand-type (type-of rand tenv)))
                       (cases type rator-type
                              (proc-type
                               (arg-type result-type)
                               (begin
                                 (check-equal-type! arg-type rand-type rand)
                                 result-type))
                              (else (report-rator-not-a-proc-type
                                     rator-type rator)))))
           (letrec-exp
            (p-result-type p-name b-var b-var-type p-body letrec-body)
            (let ((tenv-for-letrec-body
                   (extend-tenv p-name
                                (proc-type b-var-type p-result-type)
                                tenv)))
              (let ((p-body-type
                     (type-of p-body
                              (extend-tenv b-var b-var-type
                                           tenv-for-letrec-body))))
                (check-equal-type! p-body-type p-result-type p-body)
                (type-of letrec-body tenv-for-letrec-body))))
           (pair-exp (exp1 exp2)
                     (pair-type (type-of exp1 tenv)
                                (type-of exp2 tenv)))
           (unpair-exp
            (var1 var2 exp body)
            (let ((exp-type (type-of exp tenv)))
              (cases type exp-type
                     (pair-type
                      (ty1 ty2)
                      (type-of body
                               (extend-tenv var2 ty2
                                            (extend-tenv var1 ty1 tenv))))
                     (else (report-exp-not-a-pair-type exp-type exp))))))))

(define report-rator-not-a-proc-type
  (lambda (rator-type rator)
    (eopl:error 'type-of
                "Rator not a proc type:~%~s~%had rator type ~s"
                rator rator-type)))

(define report-exp-not-a-pair-type
  (lambda (exp-type exp)
    (eopl:error 'type-of
                "Unpair exp not a pair type:~%~s~%had exp type ~s"
                exp exp-type)))

;; init-tenv : () -> TEnv
(define init-tenv
  (lambda ()
    (empty-tenv)))


;;; Type environment:

(define-datatype type-environment type-environment?
  (empty-tenv)
  (extend-tenv
   (var identifier?)
   (type type?)
   (saved-tenv type-environment?)))

;; apply-tenv : TEnv * Var -> Type
(define apply-tenv
  (lambda (tenv search-var)
    (cases type-environment tenv
           (empty-tenv () (report-no-binding-found 'apply-tenv search-var))
           (extend-tenv (var type saved-tenv)
                        (if (eqv? search-var var)
                            type
                            (apply-tenv saved-tenv search-var))))))


;;; Expressed values:

(define-datatype expval expval?
  (num-val
   (num number?))
  (bool-val
   (bool boolean?))
  (proc-val
   (proc proc?))
  (pair-val
   (val1 expval?)
   (val2 expval?)))

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

;; expval->pair : ExpVal -> Pair
(define expval->pair
  (lambda (val)
    (cases expval val
           (pair-val (val1 val2) (cons val1 val2))
           (else (report-expval-extractor-error 'pair val)))))

(define report-expval-extractor-error
  (lambda (s val)
    (eopl:error s "ExpVal extractor error: ~s" val)))

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


;;; Interpreter:

;; check&run : String -> ExpVal
(define check&run
  (lambda (s)
    (let ((pgm (scan&parse s)))
      (type-of-program pgm)
      (value-of-program pgm))))

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
           (proc-exp (var var-type body)
                     (proc-val (procedure var body env)))
           (call-exp (rator rand)
                     (let ((proc (expval->proc (value-of rator env)))
                           (arg (value-of rand env)))
                       (apply-procedure proc arg)))
           (letrec-exp
            (p-result-type p-name b-var b-var-type p-body letrec-body)
            (value-of letrec-body
                      (extend-env-rec p-name b-var p-body env)))
           (pair-exp (exp1 exp2)
                     (let ((val1 (value-of exp1 env))
                           (val2 (value-of exp2 env)))
                       (pair-val val1 val2)))
           (unpair-exp (var1 var2 exp body)
                       (let ((p (expval->pair (value-of exp env))))
                         (value-of body
                                   (extend-env var2 (cdr p)
                                               (extend-env var1 (car p)
                                                           env))))))))

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
   (saved-var identifier?)
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
                      (report-no-binding-found 'apply-env search-var))
           (extend-env (saved-var saved-val saved-env)
                       (if (eqv? search-var saved-var)
                           saved-val
                           (apply-env saved-env search-var)))
           (extend-env-rec (p-name b-var body saved-env)
                           (if (eqv? search-var p-name)
                               (proc-val (procedure b-var body e))
                               (apply-env saved-env search-var))))))

(define report-no-binding-found
  (lambda (s search-var)
    (eopl:error s "No binding for ~s" search-var)))


;;; Scanner and Parser:

(define scan&parse
  (sllgen:make-string-parser the-lexical-spec the-grammar))
