#lang racket
(require (for-syntax "../syntax.rkt"
                     syntax/parse)
         (rename-in racket/base
          [+  add][- sub]
          [* mult][/ div])
         "../private/sugar.rkt")

(define-operator + add  #:prec 11 #:assoc 'left)
(define-operator - sub  #:prec 11 #:assoc 'left)
(define-operator * mult #:prec 12 #:assoc 'left)
(define-operator / div  #:prec 12 #:assoc 'right)

(define-operator (! l) #:prec 12
  #:get get-none
  (let loop ([x l][y 1])
    (if (= x 0) y
        (loop (- x 1) (* x y)))))

(define-syntax/operator #%parens
  #:tag #:keep-token
  #:prec 19
  #:get get-first
  #:combine
  (case-lambda
    [(  r) r]
    [(l r) (with-syntax ([f l][x (parse-all (drop-token r))])
             #'(f x))])
  #:expand
  (λ(stx)(parse-all (drop-token stx))))

(define-operator #%jx mult #:prec 12 #:assoc 'left)


(define-infix (=> e stx) #:prec 5
  (with-parse-bindings [(e) #f]
    (define-values (body stx*)(parse (drop-token stx) (prec-cmp <= 5)))
    (with-syntax ([x e][body body])
      (values #'(λ(x) body) stx*))))


(define-syntax (quote-parsed: stx)
  (syntax-parse stx
    [(_ stuff ...)
     (with-syntax ([stx* (parse-all #'(stuff ...))])
       #'(quote stx*))]))


(define-syntax ($ stx)
  (syntax-parse stx
    [(_ stuff ...)(parse-all #'(stuff ...))]))

