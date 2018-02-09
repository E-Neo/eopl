#lang eopl

;;; Examples:

;; % print: 12
;; var x, y, z;
;; {
;;   x = 3; y = 4; z = 0;
;;   do {
;;     z = +(z, y);
;;     x = -(x, 1)
;;   } while not(zero?(x));
;;   print z
;; }


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
  '((program (statement) a-program)
    (statement
     (identifier "=" expression)
     assign-stat)
    (statement
     ("print" expression)
     print-stat)
    (statement
     ("{" (separated-list statement ";") "}")
     block-stat)
    (statement
     ("if" expression statement statement)
     if-stat)
    (statement
     ("while" expression statement)
     while-stat)
    (statement
     ("var" (separated-list identifier ",") ";" statement)
     var-stat)
    (expression (number) const-exp)
    (expression (identifier) var-exp)
    (expression
     ("-" "(" expression "," expression ")")
     diff-exp)
    (expression
     ("zero?" "(" expression ")")
     zero?-exp)
    (expression
     ("proc" "(" (separated-list identifier ",") ")" expression)
     proc-exp)
    (expression
     ("(" expression (arbno expression) ")")
     call-exp)
    (expression
     ("not" "(" expression ")")
     not-exp)
    (expression
     ("+" "(" expression "," expression ")")
     add-exp)
    (expression
     ("*" "(" expression "," expression ")")
     mult-exp)
    (expression
     ("/" "(" expression "," expression ")")
     quotient-exp)
    (statement
     ("do" statement "while" expression)
     do-while-stat)))


;;; Syntax data types:

(define-datatype program program?
  (a-program
   (stat1 statement?)))

(define-datatype statement statement?
  (assign-stat
   (var identifier?)
   (exp1 expression?))
  (print-stat
   (exp1 expression?))
  (block-stat
   (stats (list-of statement?)))
  (if-stat
   (exp1 expression?)
   (stat1 statement?)
   (stat2 statement?))
  (while-stat
   (exp1 expression?)
   (stat1 statement?))
  (var-stat
   (vars (list-of identifier?))
   (stat1 statement?))
  (do-while-stat
   (stat1 statement?)
   (exp1 expression?)))

(define-datatype expression expression?
  (const-exp
   (num number?))
  (diff-exp
   (exp1 expression?)
   (exp2 expression?))
  (zero?-exp
   (exp1 expression?))
  (var-exp
   (var identifier?))
  (proc-exp
   (vars (list-of identifier?))
   (body expression?))
  (call-exp
   (rator expression?)
   (rands (list-of expression?)))
  (not-exp
   (exp1 expression?))
  (add-exp
   (exp1 expression?)
   (exp2 expression?))
  (mult-exp
   (exp1 expression?)
   (exp2 expression?))
  (quotient-exp
   (exp1 expression?)
   (exp2 expression?)))

(define identifier?
  (lambda (x)
    (symbol? x)))

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
                                (extend-env* vars
                                             (map newref vals)
                                             saved-env))))))


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

;; print-exp : ExpVal -> ()
(define print-exp
  (lambda (val)
    (cases expval val
           (num-val (num) (display num))
           (bool-val (bool) (display bool))
           (proc-val (proc) (display "procedure")))
    (newline)))


;;; Interpreter:

;; run : String -> ExpVal
(define run
  (lambda (s)
    (result-of-program (scan&parse s))))

;; result-of-program : Program -> ExpVal
(define result-of-program
  (lambda (pgm)
    (initialize-store!)
    (cases program pgm
           (a-program (stat1)
                      (result-of stat1 (init-env))))))

;; result-of : Stat * Env -> ()
(define result-of
  (lambda (stat env)
    (cases statement stat
           (assign-stat (var exp1)
                        (setref! (apply-env env var)
                                 (value-of exp1 env)))
           (print-stat (exp1)
                       (print-exp (value-of exp1 env)))
           (block-stat (stats)
                       (result-of-stats stats env))
           (if-stat (exp1 stat1 stat2)
                    (if (expval->bool (value-of exp1 env))
                        (result-of stat1 env)
                        (result-of stat2 env)))
           (while-stat (exp1 stat1)
                       (if (expval->bool (value-of exp1 env))
                           (begin (result-of stat1 env)
                                  (result-of (while-stat exp1 stat1) env))
                           'DONE))
           (var-stat (vars stat1)
                     (result-of stat1 (extend-env-var vars env)))
           (do-while-stat (stat1 exp1)
                          (begin (result-of stat1 env)
                                 (if (expval->bool (value-of exp1 env))
                                     (result-of (do-while-stat stat1 exp1)
                                                env)
                                     'DONE))))))

;; result-of-stats : Listof(Stat) * Env -> ()
(define result-of-stats
  (lambda (stats env)
    (if (null? stats)
        'DONE
        (begin (result-of (car stats) env)
               (result-of-stats (cdr stats) env)))))

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
           (var-exp (var) (deref (apply-env env var)))
           (proc-exp (vars body)
                     (proc-val (procedure vars body env)))
           (call-exp (rator rands)
                     (let ((proc (expval->proc (value-of rator env)))
                           (args (map (lambda (rand)
                                        (value-of rand env))
                                      rands)))
                       (apply-procedure proc args)))
           (not-exp (exp1)
                    (if (expval->bool (value-of exp1 env))
                        (bool-val #f)
                        (bool-val #t)))
           (add-exp (exp1 exp2)
                    (num-val (+ (expval->num (value-of exp1 env))
                                (expval->num (value-of exp2 env)))))
           (mult-exp (exp1 exp2)
                     (num-val (* (expval->num (value-of exp1 env))
                                 (expval->num (value-of exp2 env)))))
           (quotient-exp (exp1 exp2)
                         (num-val (quotient
                                   (expval->num (value-of exp1 env))
                                   (expval->num (value-of exp2 env))))))))

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

;; extend-env-var : Listof(Var) * Env -> Env
(define extend-env-var
  (lambda (var-list env)
    (if (null? var-list)
        env
        (let ((var (car var-list))
              (ref (newref 'uninitialized)))
          (extend-env-var (cdr var-list)
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
                           (apply-env saved-env search-var))))))

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
