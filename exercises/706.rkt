#lang eopl

;;; Examples:

;; % The value of the following program is #t
;; let x = 0
;; in letrec
;;      bool even()
;;           = if zero?(x) then zero?(0)
;;             else begin set x = -(x, 1); (odd) end
;;      bool odd()
;;           = if zero?(x) then zero?(1)
;;             else begin set x = -(x, 1); (even) end
;; in begin set x = 13; (odd) end


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
     ("proc" "(" (separated-list identifier ":" type ",") ")" expression)
     proc-exp)
    (expression
     ("(" expression (arbno expression) ")")
     call-exp)
    (expression
     ("letrec"
      (arbno type identifier
             "(" (separated-list identifier ":" type ",") ")"
             "=" expression)
      "in" expression)
     letrec-exp)
    (type ("int") int-type)
    (type ("bool") bool-type)
    (type ("(" (separated-list type "*") "->" type ")") proc-type)
    (expression
     ("begin" expression (arbno ";" expression) "end")
     begin-exp)
    (expression
     ("set" identifier "=" expression)
     assign-exp)))


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
   (vars-types (list-of type?))
   (body expression?))
  (call-exp
   (rator expression?)
   (rands (list-of expression?)))
  (letrec-exp
   (p-results-types (list-of type?))
   (p-names (list-of identifier?))
   (b-vars-list (list-of (list-of identifier?)))
   (b-vars-types-list (list-of (list-of type?)))
   (p-bodies (list-of expression?))
   (letrec-body expression?))
  (begin-exp
   (exp1 expression?)
   (exps (list-of expression?)))
  (assign-exp
   (var identifier?)
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

;; apply-procedure : Proc * Listof(ExpVal) -> ExpVal
(define apply-procedure
  (lambda (proc1 vals)
    (cases proc proc1
           (procedure (vars body saved-env)
                      (value-of body
                                (extend-env*
                                 vars (map newref vals) saved-env))))))


;;; Types:

(define-datatype type type?
  (int-type)
  (bool-type)
  (proc-type
   (args-types (list-of type?))
   (result-type type?))
  (unspecified-type))

(define type-to-external-form
  (lambda (ty)
    (cases type ty
           (int-type () 'int)
           (bool-type () 'bool)
           (proc-type (args-types result-type)
                      (append (type-list-to-external-form args-types)
                              (list '->
                                    (type-to-external-form result-type))))
           (unspecified-type () 'unspecified))))

(define type-list-to-external-form
  (lambda (types)
    (if (null? types)
        '()
        (letrec
            ((iter
              (lambda (types res)
                (if (null? types)
                    res
                    (iter
                     (cdr types)
                     (append res
                             (list '*
                                   (type-to-external-form (car types)))))))))
          (iter (cdr types)
                (list (type-to-external-form (car types))))))))


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

;; check-equal-types! : Listof(Type) * Listof(Type) * Exp -> Unspecified
(define check-equal-types!
  (lambda (tys1 tys2 exp)
    (if (null? tys1)
        'DONE
        (begin (check-equal-type! (car tys1) (car tys2) exp)
               (check-equal-types! (cdr tys1) (cdr tys2) exp)))))

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
           (let-exp (vars exps body)
                    (let ((exps-types (map (lambda (exp1)
                                             (type-of exp1 tenv))
                                           exps)))
                      (type-of body
                               (extend-tenv* vars exps-types tenv))))
           (proc-exp (vars vars-types body)
                     (let ((result-type
                            (type-of body
                                     (extend-tenv* vars vars-types tenv))))
                       (proc-type vars-types result-type)))
           (call-exp
            (rator rands)
            (let ((rator-type (type-of rator tenv))
                  (rands-types (map (lambda (rand)
                                      (type-of rand tenv))
                                    rands)))
              (cases type rator-type
                     (proc-type
                      (args-types result-type)
                      (begin
                        (check-equal-type! args-types rands-types rands)
                        result-type))
                     (else (report-rator-not-a-proc-type
                            rator-type rator)))))
           (letrec-exp
            (p-results-types
             p-names b-vars-list b-vars-types-list p-bodies letrec-body)
            (let ((tenv-for-letrec-body
                   (extend-tenv*
                    p-names
                    (map (lambda (b-vars-types p-result-type)
                           (proc-type b-vars-types p-result-type))
                         b-vars-types-list p-results-types)
                    tenv)))
              (let ((p-bodies-types
                     (map (lambda (b-vars b-vars-types p-body)
                            (type-of p-body
                                     (extend-tenv* b-vars b-vars-types
                                                   tenv-for-letrec-body)))
                          b-vars-list b-vars-types-list p-bodies)))
                (check-equal-types! p-bodies-types p-results-types p-bodies)
                (type-of letrec-body tenv-for-letrec-body))))
           (begin-exp (exp1 exps)
                      (letrec
                          ((type-of-begins
                            (lambda (e1 es)
                              (let ((ty1 (type-of e1 tenv)))
                                (if (null? es)
                                    ty1
                                    (type-of-begins (car es) (cdr es)))))))
                        (type-of-begins exp1 exps)))
           (assign-exp (var exp1) (unspecified-type)))))

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

;; extend-tenv* : Listof(Var) * Listof(Type) * TEnv -> TEnv
(define extend-tenv*
  (lambda (vars types tenv)
    (if (null? vars)
        tenv
        (let ((var (car vars))
              (ty (car types)))
          (extend-tenv* (cdr vars) (cdr types)
                        (extend-tenv var ty tenv))))))

;; apply-tenv : TEnv * Var -> Type
(define apply-tenv
  (lambda (tenv search-var)
    (cases type-environment tenv
           (empty-tenv () (report-no-binding-found 'apply-tenv search-var))
           (extend-tenv (var type saved-tenv)
                        (if (eqv? search-var var)
                            type
                            (apply-tenv saved-tenv search-var))))))


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
    (initialize-store!)
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
           (var-exp (var) (deref (apply-env env var)))
           (let-exp (vars exps body)
                    (let ((vals (map (lambda (exp1)
                                       (value-of exp1 env))
                                     exps)))
                      (value-of body
                                (extend-env* vars (map newref vals) env))))
           (proc-exp (vars vars-types body)
                     (proc-val (procedure vars body env)))
           (call-exp (rator rands)
                     (let ((proc (expval->proc (value-of rator env)))
                           (args (map (lambda (rand)
                                        (value-of rand env))
                                      rands)))
                       (apply-procedure proc args)))
           (letrec-exp
            (p-results-types
             p-names b-vars-list b-vars-types-list p-bodies letrec-body)
            (value-of letrec-body
                      (extend-env-rec* p-names b-vars-list p-bodies env)))
           (begin-exp (exp1 exps)
                      (letrec
                          ((value-of-begins
                            (lambda (e1 es)
                              (let ((v1 (value-of e1 env)))
                                (if (null? es)
                                    v1
                                    (value-of-begins (car es) (cdr es)))))))
                        (value-of-begins exp1 exps)))
           (assign-exp (var exp1)
                       (begin (setref! (apply-env env var)
                                       (value-of exp1 env))
                              (num-val 27))))))

;; init-env : () -> Env
(define init-env
  (lambda ()
    (empty-env)))


;;; Environment:

;; Env = (empty-env)
;;     | (extend-env Var Ref(ExpVal) Env)
;;     | (extend-env-rec* Listof(Var) Listof(Listof(Var)) Listof(Exp) Env)

(define-datatype environment environment?
  (empty-env)
  (extend-env
   (saved-var identifier?)
   (saved-ref reference?)
   (saved-env environment?))
  (extend-env-rec*
   (p-names (list-of identifier?))
   (b-vars-list (list-of (list-of identifier?)))
   (bodies (list-of expression?))
   (saved-env environment?)))

;; extend-env* : Listof(Var) * Listof(Ref(ExpVal)) * Env -> Env
(define extend-env*
  (lambda (var-list ref-list env)
    (if (null? var-list)
        env
        (let ((var (car var-list))
              (ref (car ref-list)))
          (extend-env* (cdr var-list) (cdr ref-list)
                       (extend-env var ref env))))))

;; apply-env : Env * Var -> Ref(ExpVal)
(define apply-env
  (lambda (e search-var)
    (cases environment e
           (empty-env ()
                      (report-no-binding-found 'apply-env search-var))
           (extend-env (saved-var saved-ref saved-env)
                       (if (eqv? search-var saved-var)
                           saved-ref
                           (apply-env saved-env search-var)))
           (extend-env-rec*
            (p-names b-vars-list bodies saved-env)
            (apply-extend-env-rec*
             e p-names b-vars-list bodies saved-env search-var)))))

;; apply-extend-env-rec* :
;;   Env * Listof(Var) * Listof(Listof(Var)) * Listof(Exp) * Env * Var
;;   -> Ref(ExpVal)
(define apply-extend-env-rec*
  (lambda (env p-names b-vars-list bodies saved-env search-var)
    (if (null? p-names)
        (apply-env saved-env search-var)
        (if (eqv? search-var (car p-names))
            (newref (proc-val (procedure (car b-vars-list) (car bodies) env)))
            (apply-extend-env-rec*
             env
             (cdr p-names) (cdr b-vars-list) (cdr bodies) saved-env
             search-var)))))

(define report-no-binding-found
  (lambda (s search-var)
    (eopl:error s "No binding for ~s" search-var)))


;;; Store:

;; empty-store : () -> Sto
(define empty-store
  (lambda () '()))

;; usage: A Scheme variable containing the current state of the store.
;;        Initially set to a dummy value.
(define the-store 'uninitialized)

;; get-store : () -> Sto
(define get-store
  (lambda () the-store))

;; initialize-store! : () -> Unspecified
;; usage: (initialize-store!) sets the-store to the empty store
(define initialize-store!
  (lambda ()
    (set! the-store (empty-store))))

;; reference? : SchemeVal -> Bool
(define reference?
  (lambda (x) (integer? x)))

;; newref : ExpVal -> Ref
(define newref
  (lambda (val)
    (let ((next-ref (length the-store)))
      (set! the-store (append the-store (list val)))
      next-ref)))

;; deref : Ref -> ExpVal
(define deref
  (lambda (ref)
    (list-ref the-store ref)))

;; setref! : Ref * ExpVal -> Unspecified
;; usage: Sets the-store to a state like the original, but with position ref
;;        containing val.
(define setref!
  (lambda (ref val)
    (set! the-store
          (letrec
              ((setref-inner
                ;; usage: (setref-inner store1 ref1) returns a list like
                ;;        store1, except that position ref1 contains val.
                (lambda (store1 ref1)
                  (cond ((null? store1)
                         (report-invalid-reference ref the-store))
                        ((zero? ref1)
                         (cons val (cdr store1)))
                        (else
                         (cons (car store1)
                               (setref-inner (cdr store1) (- ref1 1))))))))
            (setref-inner the-store ref)))))

(define report-invalid-reference
  (lambda (ref the-store)
    (eopl:error 'setref
                "illegal reference ~s in store ~s"
                ref the-store)))


;;; Scanner and Parser:

(define scan&parse
  (sllgen:make-string-parser the-lexical-spec the-grammar))
