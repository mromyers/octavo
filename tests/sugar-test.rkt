#lang racket
(require (for-syntax "../private/parse.rkt"
                     syntax/parse)
         "../private/sugar.rkt")

(define-rename-operator add  + #:prec 1 #:assoc 'left)
(define-rename-operator sub  - #:prec 1 #:assoc 'left)
(define-rename-operator mult * #:prec 2 #:assoc 'left)
(define-rename-operator div  / #:prec 2 #:assoc 'right)

(define-operator (! l) #:prec 2
  #:get (get-none)
  (let loop ([x l][y 1])
    (cond [(= x 0) y]
          [else    (loop (- x 1) (* x y))])))


(define-syntax (quote-parsed stx)
  (syntax-parse stx
    [(_ stuff ...)
     (with-syntax ([stx* (parse-all #'(stuff ...))])
       #'(quote stx*))]))
