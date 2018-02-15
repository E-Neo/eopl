#lang eopl

;;; Examples:

;; % The value of the following program is 24
;; letrec fact(n)
;;        = if zero?(n) then 1
;;          else *(n, (fact -(n, 1)))
;; in (fact 4)

;; % The value of the following program is 24
;; letrec fact-iter-acc(n)
;;        = proc (a)
;;            if zero?(n) then a
;;            else ((fact-iter-acc -(n, 1)) *(n, a))
;; in let fact-iter = proc (n)
;;                      ((fact-iter-acc n) 1)
;;    in (fact-iter 4)


;; (fact 1)
;; (apply-procedure/k proc1
;;   #(struct:num-val 1)
;;   #(struct:end-cont))
;; (apply-procedure/k proc1
;;   #(struct:num-val 0)
;;   #(struct:mult2-cont #(struct:num-val 1) #(struct:end-cont)))


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
     letrec-exp)
    (expression
     ("*" "(" expression "," expression ")")
     mult-exp)))


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
   (letrec-body expression?))
  (mult-exp
   (exp1 expression?)
   (exp2 expression?)))

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
;; procedure : Var * Exp * Env -> Proc
(define-datatype proc proc?
  (procedure
   (var identifier?)
   (body expression?)
   (saved-env environment?)))

;; apply-procedure/k : Proc * ExpVal * Cont -> FinalAnswer
(define apply-procedure/k
  (lambda (proc1 val cont)
    (eopl:printf "(apply-procedure/k proc1~%  ~s~%  ~s)~%~%"
                 val cont)
    (cases proc proc1
           (procedure (var body saved-env)
                      (value-of/k body
                                  (extend-env var val saved-env)
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
    (eopl:printf "(value-of/k ~s ~%  ~s~%  ~s)~%~%"
                 (exp->string exp) env cont)
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
           (proc-exp (var body)
                     (apply-cont cont (proc-val (procedure var body env))))
           (call-exp (rator rand)
                     (value-of/k rator env
                                 (rator-cont rand env cont)))
           (letrec-exp (p-name b-var p-body letrec-body)
                       (value-of/k letrec-body
                                   (extend-env-rec p-name b-var p-body env)
                                   cont))
           (mult-exp (exp1 exp2)
                     (value-of/k exp1 env
                                 (mult1-cont exp2 env cont))))))

(define exp->string
  (lambda (exp)
    (cases expression exp
           (const-exp (num) (number->string num))
           (diff-exp (exp1 exp2)
                     (string-append "-("
                                    (exp->string exp1)
                                    ", "
                                    (exp->string exp2)
                                    ")"))
           (zero?-exp (exp1)
                      (string-append "zero?(" (exp->string exp1) ")"))
           (if-exp (exp1 exp2 exp3)
                   (string-append "if "
                                  (exp->string exp1)
                                  " then "
                                  (exp->string exp2)
                                  " else "
                                  (exp->string exp3)))
           (var-exp (var) (symbol->string var))
           (let-exp (var exp1 body)
                    (string-append "let "
                                   (symbol->string var)
                                   " = "
                                   (exp->string exp1)
                                   " in "
                                   (exp->string body)))
           (proc-exp (var body)
                     (string-append "proc ("
                                    (symbol->string var)
                                    ") "
                                    (exp->string body)))
           (call-exp (rator rand)
                     (string-append "("
                                    (exp->string rator)
                                    " "
                                    (exp->string rand)
                                    ")"))
           (letrec-exp (p-name b-var p-body letrec-body)
                       (string-append "letrec "
                                      (symbol->string p-name)
                                      "(" (symbol->string b-var) ") "
                                      "= " (exp->string p-body)
                                      " in " (exp->string letrec-body)))
           (mult-exp (exp1 exp2)
                     (string-append "*("
                                    (exp->string exp1)
                                    ", "
                                    (exp->string exp2)
                                    ")")))))

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
   (rand expression?)
   (env environment?)
   (cont continuation?))
  (rand-cont
   (val1 expval?)
   (cont continuation?))
  (mult1-cont
   (exp2 expression?)
   (env environment?)
   (cont continuation?))
  (mult2-cont
   (val1 expval?)
   (cont continuation?)))

;; apply-cont : Cont * ExpVal -> FinalAnswer
(define apply-cont
  (lambda (cont val)
    (eopl:printf "(apply-cont~%  ~s~%  ~s)~%~%" cont val)
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
           (rator-cont (rand saved-env saved-cont)
                       (value-of/k rand saved-env
                                   (rand-cont val saved-cont)))
           (rand-cont (val1 saved-cont)
                      (let ((proc1 (expval->proc val1)))
                        (apply-procedure/k proc1 val saved-cont)))
           (mult1-cont (exp2 saved-env saved-cont)
                       (value-of/k exp2 saved-env
                                   (mult2-cont val saved-cont)))
           (mult2-cont (val1 saved-cont)
                       (let ((num1 (expval->num val1))
                             (num2 (expval->num val)))
                         (apply-cont saved-cont
                                     (num-val (* num1 num2))))))))


;;; Scanner and Parser:

(define scan&parse
  (sllgen:make-string-parser the-lexical-spec the-grammar))
