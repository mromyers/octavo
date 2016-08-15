#lang racket
(require (for-syntax "../private/token.rkt"
                     "../private/parse.rkt"
                     "../private/infix.rkt"
                     syntax/parse
                     racket/list)
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

(define-operator #%jx mult #:prec 12 #:assoc 'left)

(define (ref a i)
  (if (dict? a) (dict-ref a i) (sequence-ref a i)))
(begin-for-syntax
  (define (parse/comma stx)
    (define (next-exp lst)
      (define-values (fst rst)
        (splitf-at lst (λ(a)(not (equal? (syntax-e a) '\,)))))
      (with-syntax ([(x ...) fst])
        (values #'(#%exp x ...) rst)))
    (cond [(null? (syntax-e stx)) '()]
          [else (define-values (e₀ rst)(next-exp (syntax-e stx)))
                (cons e₀ (let loop ([rst rst])
                           (cond [(null? rst) '()]
                                 [else (define-values (e rst⁻)
                                         (next-exp (cdr rst)))
                                       (cons e (loop rst⁻))])))]))
  
  (define (get-until-sc stx)
    (define (not-sc a)(not (equal? (syntax-e a) '\;)))
    (define-values (fst rst)(splitf-at (syntax-e stx) not-sc))
    (values (datum->syntax stx fst)
            (if (null? rst) (datum->syntax stx rst)
                (datum->syntax stx (cdr rst)))))

  (define (get-params stx)
    (syntax-parse stx
      [(~or () (x:id)) stx]
      [(x₀:id (~seq (~datum \,) x₁) ...) #'(x₀ x₁ ...)]))

)

(define-infix (#%parens e stx)
  #:is-tag #t
  #:precedence 9
  (define-values (par stx⁻)(get-first stx))
  (cond [e (with-syntax ([f e][(x ...)(parse/comma (drop-token par))])
             (values #'(f x ...) stx⁻))]
        [else (with-syntax ([(x ...)(drop-token par)])
                (values #'(#%exp x ...) stx⁻))]))

(define-infix (#%brackets e stx)
  #:is-tag #t
  #:precedence 9
  (define-values (brk stx⁻)(get-first stx))
  (cond [e (with-syntax ([a e][(x ...)(parse/comma (drop-token brk))])
             (values #'(ref a x ...) stx⁻))]
        [else (with-syntax   ([(x ...)(parse/comma (drop-token brk))])
                (values #'(list x ...) stx⁻))]))


(define-infix (#%braces stx)
  #:is-tag #t
  (define-values (brc stx⁻)(get-first stx))
  (with-syntax ([(x ...)(parse/comma (drop-token brc))])
    (values #'(vector x ...) stx⁻))
  #:stmt-proc
  (λ(stx)
    (define-values (brc stx⁻)(get-first stx))
    (with-syntax ([(x ...) brc])
      (values #'(#%block x ...) stx⁻))))

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

(define-syntax (quote-parsed-block: stx)
  (syntax-parse stx
    [(_ stuff ...)
     (with-syntax ([(stmt ...)(parse-block #'(stuff ...))])
       #'(quote (stmt ...)))]))


(define-syntax (#%exp stx)
  (syntax-parse stx
    [(_ stuff ...)
     (parse-all #'(stuff ...))]))

(define-syntax (#%stmt stx)
  (syntax-parse stx
    [(_ stuff ...)
     #'(#%exp stuff ...)]))

(define-syntax (#%block stx)
  (syntax-parse stx
    [(_ stuff ...)
     (with-syntax ([(stmt ...)(parse-block #'(stuff ...))])
       #'(let () stmt ...))]))

(define-statement (def stx)
  (syntax-parse stx
    #:datum-literals (= \;)
    [(_ y = stuff ...)
     (define-values (rhs rst) (get-until-sc #'(stuff ...)))
     (with-syntax ([(x ...) rhs])
       (values #'(define y (#%exp x ...)) rst))]
    [(_ f args = stuff ...)
     (define-values (rhs rst)(get-until-sc #'(stuff ...)))
     (with-syntax ([(x ...) rhs][args* (get-params #'args)])
       (values #'(define f (lambda args* (#%exp x ...))) rst))]
    [(_ f args {body ...} stuff ...)
     (with-syntax ([(x ...) (get-params #'args)])
       (values #'(define (f x ...)(#%block body ...))
               #'(stuff ...)))]))


(#%block
  def x = 1 \;
  printf ("~a\n" \, x) \;
  def f(x) {
    printf("~a\n" \, x) \;
    printf("~a\n" \, x + 1) \;
  }
  f(x) \;
  {
    def x = 3 \;
    f(x) \;
  }
  f(x) \;
)

