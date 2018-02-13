#lang eopl

;;    (type-of exp1 tenv) = listof t
;; -----------------------------------
;;  (type-of (car-exp exp1) tenv) = t

;;       (type-of exp1 tenv) = listof t
;; ------------------------------------------
;;  (type-of (cdr-exp exp1) tenv) = listof t

;; We can't get the information whether a list is empty before we evaluate
;; the program.
;; We treat emptylist as an ordinary list so it must have a type.
;; Consider cons(1, emptylist) and cons(zero?(1), emptylist):
;; we can not do type checking for these two expression without knowing
;; the type of emptylist.


;;; Examples:

;; % The value of the following program is list (0 1 2 3 4)
;; let lst = list(1, 2, 3, 4)
;; in let fun = proc (lst : listof int)
;;                if null?(lst) then lst
;;                else cons(0, lst)
;;    in (fun lst)


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
    (type ("listof" type) list-type)
    (expression
     ("list" "(" expression (arbno "," expression) ")")
     list-exp)
    (expression
     ("cons" "(" expression "," expression ")")
     cons-exp)
    (expression
     ("null?" "(" expression ")")
     null?-exp)
    (expression
     ("emptylist_" type)
     emptylist-exp)
    (expression
     ("car" "(" expression ")")
     car-exp)
    (expression
     ("cdr" "(" expression ")")
     cdr-exp)))


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
  (list-exp
   (exp1 expression?)
   (exps (list-of expression?)))
  (cons-exp
   (exp1 expression?)
   (exp2 expression?))
  (null?-exp
   (exp1 expression?))
  (emptylist-exp
   (ty type?))
  (car-exp
   (exp1 expression?))
  (cdr-exp
   (exp1 expression?)))

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
  (list-type
   (ty type?)))

(define type-to-external-form
  (lambda (ty)
    (cases type ty
           (int-type () 'int)
           (bool-type () 'bool)
           (proc-type (arg-type result-type)
                      (list (type-to-external-form arg-type)
                            '->
                            (type-to-external-form result-type)))
           (list-type (ty)
                      (list 'listof
                            (type-to-external-form ty))))))


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
           (list-exp
            (exp1 exps)
            (letrec
                ((type-of-list
                  (lambda (exps ty)
                    (if (null? exps)
                        (list-type ty)
                        (let ((exp1 (car exps)))
                          (check-equal-type! (type-of exp1 tenv) ty exp1)
                          (type-of-list (cdr exps) ty))))))
              (type-of-list exps (type-of exp1 tenv))))
           (cons-exp (exp1 exp2)
                     (let ((ty1 (type-of exp1 tenv))
                           (ty2 (type-of exp2 tenv)))
                       (cases type ty2
                              (list-type (ty)
                                         (begin
                                           (check-equal-type! ty1 ty exp1)
                                           ty2))
                              (else (report-not-a-list-type 'cons ty2 exp2)))))
           (null?-exp
            (exp1)
            (let ((ty1 (type-of exp1 tenv)))
              (cases type ty1
                     (list-type (ty) (bool-type))
                     (else (report-not-a-list-type 'null? ty1 exp1)))))
           (emptylist-exp (ty)
                          (list-type ty))
           (car-exp (exp1)
                    (let ((ty1 (type-of exp1 tenv)))
                      (cases type ty1
                             (list-type (ty) ty)
                             (else (report-not-a-list-type 'car ty1 exp1)))))
           (cdr-exp (exp1)
                    (let ((ty1 (type-of exp1 tenv)))
                      (cases type ty1
                             (list-type (ty) (list-type ty))
                             (else (report-not-a-list-type 'cdr ty1 exp1))))))))

(define report-rator-not-a-proc-type
  (lambda (rator-type rator)
    (eopl:error 'type-of
                "Rator not a proc type:~%~s~%had rator type ~s"
                rator rator-type)))

(define report-not-a-list-type
  (lambda (s exp-type exp)
    (eopl:error s
                "Not a list type:~%~s~%had exp type ~s"
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
           (list-exp
            (exp1 exps)
            (letrec
                ((value-of-list
                  (lambda (exps vals)
                    (if (null? exps)
                        (list-val vals)
                        (value-of-list (cdr exps)
                                       (append vals
                                               (list (value-of (car exps)
                                                               env))))))))
              (value-of-list exps (list (value-of exp1 env)))))
           (cons-exp (exp1 exp2)
                     (let ((val1 (value-of exp1 env))
                           (lst (expval->list (value-of exp2 env))))
                       (list-val (cons val1 lst))))
           (null?-exp (exp1)
                      (let ((lst (expval->list (value-of exp1 env))))
                        (if (null? lst)
                            (bool-val #t)
                            (bool-val #f))))
           (emptylist-exp (ty)
                          (list-val '()))
           (car-exp (exp1)
                    (car (expval->list (value-of exp1 env))))
           (cdr-exp (exp1)
                    (cdr (expval->list (value-of exp1 env)))))))

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
