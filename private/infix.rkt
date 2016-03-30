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
  (let ([t-val (get-val #'t)])
    (if (infix? t-val)
        (infix-prec t-val)
        #f)))

(define ((prec-cmp n R) t)
  (let ([m (id-prec t)])
    (and m (n . R . m))))
#|
(define ((prec-cmp n R) t*)
  (syntax-parse t*
    [(~or t (t stuff ...))
     (let ([t-val (get-val #'t)])
       (if (infix? t-val) ((infix-prec t-val) . R . n) #f))]))
|#


(struct infix-trans (proc lbp)
  #:methods gen:infix
  [(define (infix-proc self e stx) ((infix-trans-proc self) e stx))
   (define (infix-prec self)(infix-trans-lbp self))])

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
