#lang eopl

;;; Examples:

;; 7_checked.rkt﻿> (check "if 2 then zero?(zero?(3)) else 5")
;; ; check-equal-type!: Types didn't match bool != int in
;; ; #(struct:zero?-exp #(struct:const-exp 3))

;; 707.rkt﻿> (check "if 2 then zero?(zero?(3)) else 5")
;; ; check-equal-type!: Types didn't match int != bool in
;; ; #(struct:const-exp 2)


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
    (type ("(" type "->" type ")") proc-type)))


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
   (letrec-body expression?)))

(define identifier?
  (lambda (x)
    (symbol? x)))


;;; Types:

(define-datatype type type?
  (int-type)
  (bool-type)
  (proc-type
   (arg-type type?)
   (result-type type?)))

(define type-to-external-form
  (lambda (ty)
    (cases type ty
           (int-type () 'int)
           (bool-type () 'bool)
           (proc-type (arg-type result-type)
                      (list (type-to-external-form arg-type)
                            '->
                            (type-to-external-form result-type))))))


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
                   (let ((ty1 (type-of exp1 tenv)))
                     (check-equal-type! ty1 (bool-type) exp1)
                     (let ((ty2 (type-of exp2 tenv))
                           (ty3 (type-of exp3 tenv)))
                       (check-equal-type! ty2 ty3 exp)
                       ty2)))
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
                (type-of letrec-body tenv-for-letrec-body)))))))

(define report-rator-not-a-proc-type
  (lambda (rator-type rator)
    (eopl:error 'type-of
                "Rator not a proc type:~%~s~%had rator type ~s"
                rator rator-type)))

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

(define report-no-binding-found
  (lambda (s search-var)
    (eopl:error s "No binding for ~s" search-var)))


;;; Scanner and Parser:

(define scan&parse
  (sllgen:make-string-parser the-lexical-spec the-grammar))
