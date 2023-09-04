(import (only (scheme base) define-record-type))

(define (memf f lst)
  (cond
   ((null? lst) #f)
   ((f (car lst)) lst)
   (else (memf f (cdr lst)))))

(define-syntax let-list
  (syntax-rules ()
    ((_ (() lst)
        body ...)
     (begin body ...))
    ((_ ((x y ...) lst)
        body ...)
     (let ((x (car lst)))
       (let-list ((y ...) (cdr lst))
                 body ...)))))

(define-syntax define-datatype
  (syntax-rules ()
    ((_ <type>
        (<sub-type>
         (sub-type filed ...)
         sub-type?
         (field sub-type-field)
         ...)
        ...)
     (begin
       (define-record-type <sub-type>
         (sub-type field ...)
         sub-type?
         (field sub-type-field)
         ...)
       ...
       (define <type>
         `((sub-type ,sub-type? (,sub-type-field ...))
           ...))))))

(define-syntax case-datatype
  (syntax-rules (else)
    ((_ <type> type)
     (cond
      ((memf (lambda (lst) ((cadr lst) type)) <type>)
       (error "case not covered" type))
      (else (error (string-append "not a sub-type of "
                                  (symbol->string '<type>))
                   type))))
    ((_ <type> type
        (else body1 body2 ...))
     (cond
      ((memf (lambda (lst) ((cadr lst) type)) <type>)
       (begin body1 body2 ...))
      (else (error (string-append "not a sub-type of "
                                  (symbol->string '<type>))
                   type))))
    ((_ <type> type
        ((sub-type field ...)
         body1 body2 ...)
        clause ...)
     (cond
      ((let ((lst (assv 'sub-type <type>)))
         (cond
          (lst (cond
                (((cadr lst) type) lst)
                (else #f)))
          (else (error (string-append "<"
                                      (symbol->string 'sub-type)
                                      "> is not a sub-type of "
                                      (symbol->string '<type>))
                       'sub-type))))
       =>
       (lambda (lst)
         (let ((fields (caddr lst)))
           (cond
            ((eq? (length (list 'field ...)) (length fields))
             (let-list ((field ...) (map (lambda (f) (f type)) (caddr lst)))
                       body1 body2 ...))
            (else (error "wrong number of fields" '(sub-type field ...)))))))
      (else (case-datatype <type> type clause ...))))))

(define-syntax define-parser
  (syntax-rules ()
    ((_ (parse-type x) <type>
        (<sub-type>
         sub-type sub-type? parse-sub-type?
         (field sub-type-field parse-field)
         ...)
        ...)
     (begin
       (define-datatype <type>
         (<sub-type>
          (sub-type field ...)
          sub-type?
          (field sub-type-field)
          ...)
         ...)
       (define (parse-type x)
         (cond
          (parse-sub-type?
           (sub-type parse-field ...))
          ...
          (else (error (string-append (symbol->string 'parse-type)
                                      " error")
                       x))))))))

(define-parser (sexp->expr x) <expr>
  (<const-expr>
   const-expr const-expr? (number? x)
   (num const-expr-num x))
  (<var-expr>
   var-expr var-expr? (symbol? x)
   (var var-expr-var x))
  (<diff-expr>
   diff-expr diff-expr? (and (list? x) (eq? (length x) 3)
                             (eqv? (car x) '-))
   (expr1 diff-expr-expr1 (sexp->expr (cadr x)))
   (expr2 diff-expr-expr2 (sexp->expr (caddr x))))
  (<zero?-expr>
   zero?-expr zero?-expr? (and (list? x) (eq? (length x) 2)
                               (eqv? (car x) 'zero?))
   (expr1 zero?-expr-expr1 (sexp->expr (cadr x))))
  (<if-expr>
   if-expr if-expr? (and (list? x) (eq? (length x) 4)
                         (eqv? (car x) 'if))
   (expr1 if-expr-expr1 (sexp->expr (cadr x)))
   (expr2 if-expr-expr2 (sexp->expr (caddr x)))
   (expr3 if-expr-expr3 (sexp->expr (cadddr x))))
  (<let-expr>
   let-expr let-expr? (and (list? x) (eq? (length x) 3)
                           (eqv? (car x) 'let)
                           (pair? (cadr x)) (null? (cdadr x))
                           (list? (caadr x)) (eq? (length (caadr x)) 2))
   (var let-expr-var (caaadr x))
   (expr1 let-expr-expr1 (sexp->expr (cadr (caadr x))))
   (body let-expr-body (sexp->expr (caddr x))))
  (<lambda-expr>
   lambda-expr lambda-expr? (and (list? x) (eq? (length x) 3)
                                 (eqv? (car x) 'lambda)
                                 (pair? (cadr x)) (null? (cdadr x)))
   (var lambda-expr-var (caadr x))
   (body lambda-expr-body (sexp->expr (caddr x))))
  (<call-expr>
   call-expr call-expr? (and (list? x) (eq? (length x) 2))
   (func call-expr-func (sexp->expr (car x)))
   (arg call-expr-arg (sexp->expr (cadr x))))
  (<letrec-expr>
   letrec-expr letrec-expr? (and (list? x) (eq? (length x) 3)
                                 (eqv? (car x) 'letrec)
                                 (pair? (cadr x)) (null? (cdadr x))
                                 (list? (caadr x)) (eq? (length (caadr x)) 2)
                                 (let ((init (cadr (caadr x))))
                                   (and (list? init) (eq? (length init) 3)
                                        (eqv? (car init) 'lambda)
                                        (pair? (cadr init))
                                        (null? (cdadr init)))))
   (f-name letrec-expr-f-name (caaadr x))
   (f-var letrec-expr-f-var (caadr (cadr (caadr x))))
   (f-body letrec-expr-f-body (sexp->expr (caddr (cadr (caadr x)))))
   (body letrec-expr-body (sexp->expr (caddr x)))))

(define-datatype <val>
  (<num-val>
   (num-val num)
   num-val?
   (num num-val-num))
  (<bool-val>
   (bool-val bool)
   bool-val?
   (bool bool-val-bool))
  (<lambda-val>
   (lambda-val var body env)
   lambda-val?
   (var lambda-val-var)
   (body lambda-val-body)
   (env lambda-val-env)))

(define-datatype <env>
  (<empty-env>
   (empty-env)
   empty-env?)
  (<extend-env>
   (extend-env var val env)
   extend-env?
   (var extend-env-var)
   (val extend-env-val)
   (env extend-env-env))
  (<extend-env-rec>
   (extend-env-rec f-name f-var f-body env)
   extend-env-rec?
   (f-name extend-env-rec-f-name)
   (f-var extend-env-rec-f-var)
   (f-body extend-env-rec-f-body)
   (env extend-env-rec-env)))

(define-datatype <cont>
  (<end-cont>
   (end-cont)
   end-cont?)
  (<diff1-cont>
   (diff1-cont expr2 env cont)
   diff1-cont?
   (expr2 diff1-cont-expr2)
   (env diff1-cont-env)
   (cont diff1-cont-cont))
  (<diff2-cont>
   (diff2-cont val1 cont)
   diff2-cont?
   (val1 diff2-cont-val1)
   (cont diff2-cont-cont))
  (<zero1-cont>
   (zero1-cont cont)
   zero1-cont?
   (cont zero1-cont-cont))
  (<if-test-cont>
   (if-test-cont expr2 expr3 env cont)
   if-test-cont?
   (expr2 if-test-cont-expr2)
   (expr3 if-test-cont-expr3)
   (env if-test-cont-env)
   (cont if-test-cont-cont))
  (<let-expr-cont>
   (let-expr-cont var body env cont)
   let-expr-cont?
   (var let-expr-cont-var)
   (body let-expr-cont-body)
   (env let-expr-cont-env)
   (cont let-expr-cont-cont))
  (<call-func-cont>
   (call-func-cont arg env cont)
   call-func-cont?
   (arg call-func-cont-arg)
   (env call-func-cont-env)
   (cont call-func-cont-cont))
  (<call-arg-cont>
   (call-arg-cont func cont)
   call-arg-cont?
   (func call-arg-cont-func)
   (cont call-arg-cont-cont)))

;; (-> <val> Number)
(define (val->num val)
  (case-datatype
   <val> val
   ((num-val num) num)
   (else (error "cannot convert to number" val))))

;; (-> <val> Boolean)
(define (val->bool val)
  (case-datatype
   <val> val
   ((bool-val bool) bool)
   (else (error "cannot convert to boolean" val))))

;; (-> <val> <val> <cont> <val>)
(define (apply-lambda val arg-val cont)
  (case-datatype
   <val> val
   ((lambda-val var body env)
    (value-of/k body (extend-env var arg-val env) cont))
   (else (error "cannot convert to lambda" val))))

;; (-> <env> Symbol <val>)
(define (apply-env env var)
  (case-datatype
   <env> env
   ((empty-env) (error "unbound identifier" var))
   ((extend-env saved-var val saved-env)
    (cond
     ((eqv? var saved-var) val)
     (else (apply-env saved-env var))))
   ((extend-env-rec f-name f-var f-body saved-env)
    (cond
     ((eqv? var f-name) (lambda-val f-var f-body env))
     (else (apply-env saved-env var))))))

;; (-> <cont> <val> <val>)
(define (apply-cont cont val)
  (case-datatype
   <cont> cont
   ((end-cont)
    (display "end of computation")
    (newline)
    val)
   ((diff1-cont expr2 saved-env saved-cont)
    (value-of/k expr2 saved-env (diff2-cont val saved-cont)))
   ((diff2-cont val1 saved-cont)
    (apply-cont saved-cont (num-val (- (val->num val1) (val->num val)))))
   ((zero1-cont saved-cont)
    (apply-cont saved-cont (bool-val (zero? (val->num val)))))
   ((if-test-cont expr2 expr3 saved-env saved-cont)
    (if (val->bool val)
        (value-of/k expr2 saved-env saved-cont)
        (value-of/k expr3 saved-env saved-cont)))
   ((let-expr-cont var body saved-env saved-cont)
    (value-of/k body (extend-env var val saved-env) saved-cont))
   ((call-func-cont arg saved-env saved-cont)
    (value-of/k arg saved-env (call-arg-cont val saved-cont)))
   ((call-arg-cont func saved-cont)
    (apply-lambda func val saved-cont))))

;; (-> <cont> <env> <expr> <val>)
(define (value-of/k expr env cont)
  (case-datatype
   <expr> expr
   ((const-expr num)
    (apply-cont cont (num-val num)))
   ((var-expr var)
    (apply-cont cont (apply-env env var)))
   ((diff-expr expr1 expr2)
    (value-of/k expr1 env (diff1-cont expr2 env cont)))
   ((zero?-expr expr1)
    (value-of/k expr1 env (zero1-cont cont)))
   ((if-expr expr1 expr2 expr3)
    (value-of/k expr1 env (if-test-cont expr2 expr3 env cont)))
   ((let-expr var expr1 body)
    (value-of/k expr1 env (let-expr-cont var body env cont)))
   ((lambda-expr var body)
    (apply-cont cont (lambda-val var body env)))
   ((call-expr func arg)
    (value-of/k func env (call-func-cont arg env cont)))
   ((letrec-expr f-name f-var f-body body)
    (value-of/k body (extend-env-rec f-name f-var f-body env) cont))))

;; (-> S-Exp <val>)
(define (interp x)
  (value-of/k (sexp->expr x) (empty-env) (end-cont)))
