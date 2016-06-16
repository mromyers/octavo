#lang racket
(require (for-syntax
          "../syntax.rkt"
          "../syntax/operator.rkt"
          "../syntax/token.rkt"
          syntax/parse))


(define-syntax (qt-p: stx)
  (with-syntax ([stx* (parse-all (drop-token stx))])
       #'(quote stx*)))

(define-syntax (test-cmp: stx)
  (define (rel r)
    (syntax-parse r
      [(~datum <) <] [(~datum <=) <=]
      [(~datum =) =]
      [(~datum >) >] [(~datum >=) >=]))
  (syntax-parse stx
    [(_ x:id R n:number)
     (define n* (syntax->datum #'n))
     (define p ((prec-cmp (rel #'R) n*) #'x))
     (if p #'#t #'#f)]))

(define-syntax-rule (def-tok name stuff ...)
  (define-syntax name (make-token stuff ...)))

(define-syntax-rule (def-op name stuff ...)
  (define-syntax name (make-operator stuff ...)))


(define-syntax (def-op/r stx)
  (syntax-parse stx
    [(_ name a:id prec:number)
     (with-syntax ([get (case (syntax->datum #'a)
                          [ (l) #'(get-<= prec)]
                          [ (r) #'(get-<  prec)]
                          [(pf) #'(get-none)   ])])
       #'(define-syntax name
           (make-rename-operator #:id #'name #:get get #:precedence prec)))]
    [(_ name stuff ...)
     #'(define-syntax name (make-rename-operator stuff ...))]))

(define-syntax-rule (def-tag name prec t)
  (define-syntax name
    (make-operator #:tag #t #:precedence prec
                   #:drop-token #f #:get (get-first)
      #:combine
      (case-lambda
        [  (r)(with-syntax      ([r* (parse-all (drop-token r))])
                #'(t   r*))]
        [(l r)(with-syntax ([l l][r* (parse-all (drop-token r))])
                #'(t l r*))]))))


(provide qt-p:
         def-op/r def-tag
         def-tok def-op)
