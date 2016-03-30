#lang racket/base

(require syntax/parse)

(define-values (prop:token token? token-proc)
  (make-struct-type-property 'token))

(define (atom-proc e stx dom?)
  (if e (values e stx)
      (syntax-parse stx
        [(t₀ t₁ ...)
         (parse #'t₀ #'(t₁ ...) dom?)])))


(define (parse e stx dom?)
  (syntax-parse stx
    [(~or ((t₀ x ...) t₁ ...) (t₀ t₁ ...))
     (if (and (dom? #'t₀) e) (values e stx)
         (let ([t₀-val (id-val #'t₀)])
           (if (token? t₀-val)
               ((token-proc t₀-val) t₀-val e stx dom?)
               (atom-proc e stx dom?))))]
    [() (values e stx)]))

(define (id-val t)
  (if (identifier? t)
      (syntax-local-value t (λ()#f))
      #f))

(provide parse prop:token)
