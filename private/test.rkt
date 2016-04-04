#lang racket
(require "def.rkt"
         (for-syntax "parse.rkt"
                     "infix.rkt"
                     syntax/parse))

(define-syntax (#%parse stx)
  (syntax-parse stx
    [(_ stuff ...) (parse-all #'(stuff ...))]))

(define-syntax (quote-parsed stx)
  (syntax-parse stx
    [(_ stuff ...)
     (with-syntax ([stx* (parse-all #'(stuff ...))])
       #'(quote stx*))]))

(define-syntax/operator add #:prec 1
  #:combine (λ(e₀ e₁) (with-syntax ([e₀ e₀][e₁ e₁]) #'(+ e₀ e₁)))
  #:get (get-<= 1))

(define-syntax/operator sub #:prec 1
  #:combine
  (case-lambda
    [(e₁)(with-syntax ([e₁ e₁]) #'(- e₁))]
    [(e₁ e₂)(with-syntax ([e₁ e₁][e₂ e₂]) #'(- e₁ e₂))])
  #:get (get-<= 1))

(define-syntax/operator mult #:prec 2
  #:combine (λ(e₀ e₁) (with-syntax ([e₀ e₀][e₁ e₁]) #'(* e₀ e₁)))
  #:get (get-<= 2))

(define-syntax/tag-infix (#%parens e stx) #:prec 9
  (if e (with-syntax ([e e][((#%paren stuff ...) t₁ ...) stx])
          (with-syntax ([(stuff* ...)
                         (map  (λ(stx)(parse-all stx))  
                               (keyword-split '\, #'(stuff ...)))])
            (values #'(e stuff* ...) #'(t₁ ...))))
      (with-syntax ([(t₀ t₁ ...) stx])
        (values #'t₀ #'(t₁ ...))))
  #:expand (λ(stx)(with-syntax ([(t₀ stuff ...) stx])
                    (parse-all #'(stuff ...)))))

(define-syntax/tag-infix (#%brackets e stx) #:prec 9
  (if e (with-syntax ([e e][((#%brackets stuff ...) t₁ ...) stx])
          (with-syntax ([stuff* (parse-all #'(stuff ...))])
            (values #'(sequence-ref e stuff*) #'(t₁ ...))))
      (with-syntax ([(t₀ t₁ ...) stx])
        (values #'t₀ #'(t₁ ...))))
  #:expand (λ(stx)(with-syntax ([(t₀ stuff ...) stx])
                    (with-syntax ([(stuff* ...)
                                   (map parse-all
                                        (keyword-split '\, #'(stuff ...)))])
                    #'(list stuff* ...)))))

