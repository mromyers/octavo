#lang racket
(require (for-syntax "../private/parse.rkt")
         "util.rkt"
         rackunit)

(define-syntax-rule (check-parsed-equal? (unparsed ...) (parsed ...))
  (check-equal? (qt-p: unparsed ...) (quote (parsed ...))))

(def-tok a=> #:precedence 3
  (λ(e stx)
    (define-values (a* stx+) ((get-first) stx))
    (with-parse-bindings [((datum->syntax a* 'a)) #f]
      (define-values (e* stx*) (parse-cmp e stx+ < 3))
      (with-syntax ([e₁ e*])
        (values #'(a=> e₁) stx*)))))

(def-op/r ol1 l 1)
(def-op/r ol2 l 2)
(def-op/r ol3 l 3)
(def-op/r ol4 l 4)

(def-op/r a l 1)

(check-parsed-equal?
 [a=> a ol3 a] (a=> (ol3 a a)))

(check-parsed-equal?
 [  a=> a    ol2   b    a   c ]
 [((a=> a) . ol2 . b) . a . c ])

(check-parsed-equal?
 [ a=>  a   ol3   a   ol1  a 1 ]
 [(a=> (a . ol3 . a)) . ol1 . (a 1)])

