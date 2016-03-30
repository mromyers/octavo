#lang racket
(require "def.rkt"
         (for-syntax "parse.rkt"
                     "infix.rkt"
                     syntax/parse))

(define-syntax (#%parse stx)
  (syntax-parse stx
    [(_ stuff ...) (let-values ([(e* stx*) (parse #f #'(stuff ...) (λ(t) #f))])
                     (with-syntax ([e* e*]) #'(quote e*)))]))

(define-syntax/operator add #:prec 1
  #:combine (λ(e₀ e₁) (with-syntax ([e₀ e₀][e₁ e₁]) #'(+ e₀ e₁)))
  #:get (get-<= 1))

(define-syntax/operator sub #:prec 1
  #:combine
  (case-lambda
    [(e₁)(with-syntax ([e₁ e₁]) #'(- e₁))]
    [(e₁ e₂)(with-syntax ([e₁ e₁][e₂ e₂]) #'(- e₁))])
  #:get (get-<= 1))

(define-syntax/operator mult #:prec 2
  #:combine (λ(e₀ e₁) (with-syntax ([e₀ e₀][e₁ e₁]) #'(* e₀ e₁)))
  #:get (get-<= 2))

(equal? '(+ 1 (* 2 (- 3))) (#%parse 1 add 2 mult sub 3))
