#lang eopl

;;; Examples:

;; % The value of the following program is 3
;; let x = 0
;; in let a = x
;;        b = begin
;;              set x = 1;
;;              x
;;            end
;;        c = begin
;;              set x = 2;
;;              x
;;            end
;;    in -(a, -(0, -(b, -(0, c))))

;; % The value of the following program is -1
;; let g = let count = 0
;;         in proc (dummy)
;;              begin
;;                set count = -(count, -1);
;;                count
;;              end
;; in let a = (g 11)
;;        b = (g 11)
;;    in -(a, b)


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
     ("proc" "(" identifier ")" expression)
     proc-exp)
    (expression
     ("(" expression expression ")")
     call-exp)
    (expression
     ("letrec"
      (arbno identifier "(" identifier ")" "=" expression)
      "in" expression)
     letrec-exp)
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
   (var identifier?)
   (body expression?))
  (call-exp
   (rator expression?)
   (rand expression?))
  (letrec-exp
   (proc-names (list-of identifier?))
   (bound-vars (list-of identifier?))
   (proc-bodies (list-of expression?))
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
                      (value-of body
                                (extend-env var (newref val) saved-env))))))


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
                    (let ((refs (map (lambda (exp1)
                                       (newref (value-of exp1 env)))
                                     exps)))
                      (value-of body
                                (extend-env* vars refs env))))
           (proc-exp (var body)
                     (proc-val (procedure var body env)))
           (call-exp (rator rand)
                     (let ((proc (expval->proc (value-of rator env)))
                           (arg (value-of rand env)))
                       (apply-procedure proc arg)))
           (letrec-exp (proc-names bound-vars proc-bodies letrec-body)
                       (value-of letrec-body
                                 (extend-env-rec*
                                  proc-names bound-vars proc-bodies env)))
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
;;     | (extend-env-rec* Listof(Var) Listof(Var) Listof(Exp) Env)

(define-datatype environment environment?
  (empty-env)
  (extend-env
   (saved-var symbol?)
   (saved-ref reference?)
   (saved-env environment?))
  (extend-env-rec*
   (p-names (list-of identifier?))
   (b-vars (list-of identifier?))
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
                      (report-no-binding-found search-var))
           (extend-env (saved-var saved-ref saved-env)
                       (if (eqv? search-var saved-var)
                           saved-ref
                           (apply-env saved-env search-var)))
           (extend-env-rec* (p-names b-vars bodies saved-env)
                            (apply-extend-env-rec*
                             e p-names b-vars bodies saved-env search-var)))))

;; apply-extend-env-rec* :
;;   Env * Listof(Var) * Listof(Var) * Listof(Exp) * Env * Var -> Ref(ExpVal)
(define apply-extend-env-rec*
  (lambda (env p-names b-vars bodies saved-env search-var)
    (if (null? p-names)
        (apply-env saved-env search-var)
        (if (eqv? search-var (car p-names))
            (newref (proc-val (procedure (car b-vars) (car bodies) env)))
            (apply-extend-env-rec*
             env
             (cdr p-names) (cdr b-vars) (cdr bodies) saved-env
             search-var)))))

(define report-no-binding-found
  (lambda (search-var)
    (eopl:error 'apply-env "No binding for ~s" search-var)))


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
