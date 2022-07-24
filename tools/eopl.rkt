#lang typed/racket

(require (for-syntax racket/syntax
                     syntax/parse))

(define-syntax define-datatype
  (lambda (stx)
    (define count-id
      (lambda (pat id)
        (cond
          [(symbol? pat) (if (eqv? pat id) 1 0)]
          [(null? pat) 0]
          [else (+ (count-id (car pat) id)
                   (count-id (cdr pat) id))])))
    (define pattern->type
      (lambda (pat id-tys)
        (cond
          [(symbol? pat) (hash-ref id-tys pat (lambda () `(quote ,pat)))]
          [(null? pat) 'Null]
          [else (cons 'List (map (lambda (p) (pattern->type p id-tys))
                                 pat))])))
    (define pattern->constructor
      (lambda (pat ids)
        (cond
          [(symbol? pat) (if (memv pat ids)
                             (list 'unquote pat)
                             pat)]
          [(null? pat) '()]
          [else (cons (pattern->constructor (car pat) ids)
                      (pattern->constructor (cdr pat) ids))])))
    (define pattern->extractor
      (lambda (exp pat id)
        (cond
          [(symbol? pat) (if (eqv? pat id) exp #f)]
          [(null? pat) #f]
          [else (or (pattern->extractor `(car ,exp) (car pat) id)
                    (pattern->extractor `(cdr ,exp) (cdr pat) id))])))
    (define-syntax-class ty
      #:attributes (downcase downcase.?)
      (pattern x:id
               #:with downcase
               (format-id #'x (string-downcase (symbol->string
                                                (syntax->datum #'x))))
               #:with downcase.?
               (format-id #'x "~a?"
                          (string-downcase (symbol->string
                                            (syntax->datum #'x))))))
    (define-syntax-class subty
      #:attributes (name name.downcase name.downcase.? ty ctor
                         (name... 1) (->fld 1) (fld-ty 1) (->lambda 1))
      (pattern (name:ty pat:expr [fld:id : fld-ty:expr] ...)
               #:fail-when
               (ormap (lambda (id)
                        (not (eqv? (count-id (syntax->datum #'pat) id) 1)))
                      (syntax->datum #'(fld ...)))
               "each variable should be used exactly once"
               #:with ty
               (datum->syntax
                stx
                (pattern->type (syntax->datum #'pat)
                               (make-hasheqv (syntax->datum
                                              #'((fld . fld-ty) ...)))))
               #:with ctor
               (datum->syntax
                stx
                `(lambda ,(syntax->datum #'(fld ...))
                   ,(list 'quasiquote
                          (pattern->constructor (syntax->datum #'pat)
                                                (syntax->datum #'(fld ...))))))
               #:with (name... ...)
               (datum->syntax
                stx
                (map (lambda (f) #'name) (syntax->list #'(fld ...))))
               #:with (->fld ...)
               (datum->syntax
                stx
                (map (lambda (f)
                       (format-id stx "~a->~a" #'name.downcase f))
                     (syntax->list #'(fld ...))))
               #:with (->lambda ...)
               (datum->syntax
                stx
                (map (lambda (f)
                       #`(lambda (x)
                           #,(pattern->extractor
                              'x
                              (syntax->datum #'pat)
                              (syntax-e f))))
                     (syntax->list #'(fld ...))))))
    (syntax-parse stx
      [(_ ty:ty subty:subty ...+)
       #'(begin
           (define-type ty (U subty.name ...))
           (define-predicate ty.downcase.? ty)
           (define-type subty.name subty.ty) ...
           (define-predicate subty.name.downcase.? subty.name) ...
           (define subty.name.downcase : [-> subty.fld-ty ... subty.name]
             subty.ctor) ...
           (define subty.->fld : (-> subty.name... subty.fld-ty)
             subty.->lambda) ... ...)])))
