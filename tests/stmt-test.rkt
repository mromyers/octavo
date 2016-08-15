#lang racket
(require (for-syntax "../private/token.rkt"
                     "../private/parse.rkt"
                     "../private/infix.rkt"
                     syntax/parse
                     syntax/stx

                     ))


(define-syntax (#%block stx)
  (syntax-parse stx
    [(_ stuff ...)
     (with-syntax ([(stmt ...)(parse-block #'(stuff ...))])
       #'(pretty-print (quote (begin stmt ...))))]))

(define-syntax (#%stmt stx)
  (syntax-parse stx
    [(_ stuff ...)
     #'(displayln (quote (stuff ...)))]))

(define-syntax IF
  (make-token #:stmt-proc
              (λ(stx)
                (define-values (props rst0)(get-first (drop-token stx)))
                (define-values (stmt1 rst1)(get-statement rst0))
                (cond [(or (stx-null? rst1)
                           (equal? 'ELSE (syntax-e (stx-car rst1))))
                       (define-values (stmt2 rst2)(get-statement (drop-token rst1)))
                       (define if-stmt
                         (with-syntax ([props props][stmt1 stmt1][stmt2 stmt2])
                           #'(if (#%exp props) stmt1 stmt2)))
                       (values if-stmt rst2)]
                      [else
                       (define if-stmt
                         (with-syntax ([props props][stmt1 stmt1])
                           #'(if (#%exp props) stmt1 (void))))
                       (values if-stmt rst1)]))))


(define-syntax FOO
  (make-token #:stmt-proc
              (λ(stx)
                (define-values (stmt rst) (get-statement (drop-token stx)))
                (with-syntax ([stmt stmt][rst rst])
                  (values #'(printf "FOO got this: ~a\n" (quote stmt)) #'rst)))))

#|
(define-syntax 
  (make-token #:is-tag #t
              #:stmt-proc
              (λ(stx)
                (define-values (brc stx⁻)(get-first stx))
                (with-syntax ([])

                  )
                )

              ))
|#

(#%block
 a b c d e f \;
 g h i j k l \;
 l m n o p \;
 q r \;
 return 5 \;
 FOO do stuff now \;
 IF TRUE
 IF (x == 2) print "bar" \;
 ELSE print "qaz" \;
 IF (x == 1) print "foo" \;
 a b c d e \;
)
