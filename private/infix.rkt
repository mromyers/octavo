#lang racket
(require "parse.rkt"
         racket/generic
         syntax/parse)

(define-generics infix
  [infix-proc infix e stx]
  [infix-prec infix]
  #:derive-property prop:token
  (λ(self e stx dom?)
    (let-values ([(e* stx*)(infix-proc self e stx)])
      (parse e* stx* dom?))))

(define (get-val t)
  (if (identifier? t)
      (syntax-local-value t (λ()#f))
      #f))

(define (id-prec t)
  (let ([t-val (get-val t)])
    (if (infix? t-val)
        (infix-prec t-val)
        #f)))

(define ((prec-cmp n R) t)
  (syntax-parse t
    [(~or t₀ (t₀ stuff ...))
     (let ([m (id-prec t)])
       (and m (m . R . n)))]))

(struct infix-trans (proc lbp)
  #:methods gen:infix
  [(define (infix-proc self e stx) ((infix-trans-proc self) e stx))
   (define (infix-prec self)(infix-trans-lbp self))])

(struct tag-infix-trans (proc lbp expand)
  #:property prop:procedure (struct-field-index expand)
  #:property prop:tag-token #t
  #:methods gen:infix
  [(define (infix-proc self e stx) ((tag-infix-trans-proc self) e stx))
   (define (infix-prec self)(tag-infix-trans-lbp self))])

(struct operator (comb get prec)
  #:property prop:procedure
  (λ(self stx)(apply (operator-comb self) (cdr (syntax-e stx))))
  #:methods gen:infix
  [(define (infix-proc self e stx)
     (with-syntax ([(o:id t₀ ...) stx])
       (let-values ([(es stx*)((operator-get self) #'(t₀ ...))])
         (if e
             (values ((operator-comb self) e es) stx*)
             (values ((operator-comb self) es) stx*)))))
   (define (infix-prec self)(operator-prec self))])


(define ((get-< n) stx)
  (parse #f stx (prec-cmp n <)))

(define ((get-<= n) stx)
  (parse #f stx (prec-cmp n <=)))



(provide (all-defined-out))
