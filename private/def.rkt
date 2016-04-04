#lang racket/base
(require (for-syntax racket/base
                     syntax/parse
                     "parse.rkt"
                     "infix.rkt"))

(define-syntax (define-syntax/infix stx)
  (syntax-parse stx
    [(_ id:id (~or #:prec #:precedence) n fun)
     #'(define-syntax id (infix-trans fun n))]
    [(_ (id:id args ...) (~or #:prec #:precedence) n
        body ...)
     #'(define-syntax id (infix-trans (λ(args ...) body ...) n))]))

(define-syntax (define-syntax/tag-infix stx)
  (syntax-parse stx
    [(_ id:id (~or #:prec #:precedence) n
        #:parse fun
        #:expand efun)
     #'(define-syntax id (tag-infix-trans fun n efun))]
    [(_ (id:id args ...) (~or #:prec #:precedence) n
        body ...
        #:expand efun)
     #'(define-syntax id (tag-infix-trans (λ(args ...) body ...) n efun))]))

(define-syntax (define-syntax/operator stx)
  (syntax-parse stx
    [(_ id:id
        (~or (~once (~seq (~or #:prec #:precedence) n))
             (~once (~seq #:get gfun))
             (~once (~seq (~or #:comb #:combine)
                          cfun)))
        ...)
     #'(define-syntax id (operator cfun gfun n))]))


(provide define-syntax/infix
         define-syntax/tag-infix
         define-syntax/operator)
