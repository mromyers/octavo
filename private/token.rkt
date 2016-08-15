#lang racket/base

(define-values (prop:infix-transformer
                infix-transformer?
                infix-transformer)
  (make-struct-type-property 'infix-transformer))
(define-values (prop:tag-infix-transformer
                tag-infix-transformer?
                tag-infix-transformer)
  (make-struct-type-property 'tag-infix-transformer #f
                             (list (cons prop:infix-transformer (λ(x) x)))))
(define-values (prop:statement-transformer
                statement-transformer?
                statement-transformer)
  (make-struct-type-property 'statement-transformer))

(define-values (prop:precedence
                precedence?
                precedence)
  (make-struct-type-property 'precedence))


(struct token (ex prec)
  #:property prop:procedure
  (struct-field-index ex)
  #:property prop:precedence
  (λ(self)(token-prec self)))

(struct infix-token token (iproc)
  #:property prop:infix-transformer
  (λ(self e stx)((infix-token-iproc self) e stx)))

(struct tag-infix-token token (iproc)
  #:property prop:tag-infix-transformer
  (λ(self e stx)((tag-infix-token-iproc self) e stx)))

(struct stmt+infix-token infix-token (sproc)
  #:property prop:statement-transformer
  (λ(self stx)((stmt+infix-token-sproc self) stx)))

(struct stmt+tag-infix-token tag-infix-token (sproc)
  #:property prop:statement-transformer
  (λ(self stx)((stmt+tag-infix-token-sproc self) stx)))


(define (make-token #:expand     [ex   no-expand-error]
                    #:infix-proc [ip   no-infix-error]
                    #:stmt-proc  [sp   #f]
                    #:precedence [prec #f]
                    #:is-tag     [tg   #f])
  (cond [sp   (cond [tg   (stmt+tag-infix-token ex prec ip sp)]
                    [else (stmt+infix-token     ex prec ip sp)])]
        [else (cond [tg   (tag-infix-token      ex prec ip   )]
                    [else (infix-token          ex prec ip   )])]))

(provide no-expand-error no-infix-error)
(define (no-expand-error stx)
  (raise-syntax-error #f
    "You shouldn't be using this as a macro.\
 Also, tell the author they need a better error message."))

(define (no-infix-error stx)
  (raise-syntax-error #f
    "You shouldn't be using this as an infix.\
 Also, tell the author they need a better error message."))

(provide 
 prop:infix-transformer     infix-transformer?     infix-transformer
 prop:tag-infix-transformer tag-infix-transformer? tag-infix-transformer
 prop:statement-transformer statement-transformer? statement-transformer

 prop:precedence            precedence?            precedence

 make-token)
