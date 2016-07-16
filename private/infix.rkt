#lang racket/base
(require (for-syntax racket/syntax syntax/parse)
         racket/syntax syntax/parse
         "parse.rkt")

;; precedence
(define-values (prop:precedence has-prec? prec-ref)
  (make-struct-type-property 'precedence))

(provide prec-cmp)
(define (token-prec v)
  (if (has-prec? v) ((prec-ref v) v) #f))

(define ((prec-cmp R m) t v)
  (and (has-prec? v)
       (let ([n ((prec-ref v) v)])
         (n . R . m))))

;; 'get' functions
(define (drop-token stx)
  (datum->syntax stx (cdr (syntax-e stx))))

(define ((get-dom dom?) stx)
  (parse #f stx dom?))
(define ((get-cmp R m) stx)
  (parse #f stx (prec-cmp R m)))
(define (get-<  m) (get-cmp <  m))
(define (get-<= m) (get-cmp <= m))
(define (get-none stx) (values #f stx))
(define (get-first stx)
  (let ([stx-e (syntax-e stx)])
    (values (car stx-e) (datum->syntax stx (cdr stx-e)))))

;; make infix
(struct infix-struct (proc ex prec)
  #:property prop:infix
  (λ(self e stx)((infix-struct-proc self) e stx))
  #:property prop:procedure
  (struct-field-index ex)
  #:property prop:precedence
  (λ(self)(infix-struct-prec self)))

(struct tag-infix-struct infix-struct ()
  #:property prop:tag-infix 0)

;; general token constructor
(define (make-infix proc
               #:tag        [tg   #f]
               #:precedence [prec #f]
               #:expand     [ex   no-expand-error])
  (cond [tg   (tag-infix-struct proc ex prec)]
        [else (infix-struct     proc ex prec)]))


(define (no-expand-error stx)
  (raise-syntax-error #f
    "You shouldn't be using this as a macro.\
 Also, tell the author they need a better error message."))


(provide make-operator make-rename-operator)
;; misc utility
(define (list-wrap e)(if e (list e) '()))

;; operator
(define (make-operator
         #:combine     com
         #:get         get
         #:precedence  prec
         #:tag         [tg #f]
         #:drop-token  [dt #t]
         #:com-cases   [cc #t]
         #:expand      [ex (com->ex com)])
  (make-infix #:tag tg #:precedence prec #:expand ex
              (make-operator-proc #:combine   com #:get      get
                                  #:drop-token dt #:com-cases cc)))

(define (make-operator-proc
         #:combine     com 
         #:get         get
         #:drop-token  [dt #t]
         #:com-cases   [cc #t])
  (define maybe-drop (if dt drop-token (λ(stx) stx)))
  (define com* (if cc (com-case com) com))
  (λ(e stx)
    (define-values (e* stx*)(get (maybe-drop stx)))
    (values (com* e e*) stx*)))

(define ((com-case com) l r)
  (cond[l    (cond[r    (com l r)]
                  [else (com l  )])]
       [else (cond[r    (com   r)]
                  [else (com    )])]))

(define ((com->ex com) stx)
  (syntax-parse stx
    [(_ (e₀) e₁) (com #'e₀ #'e₁)]
    [(_ (  ) e₁) (com      #'e₁)]
    [(_ (e₀)   ) (com #'e₀     )]
    [(_ (  )   ) (com          )]))

(define (make-rename-operator
         #:id         id
         #:get        get
         #:precedence prec
         #:wrap-e     [we #f]
         #:ctx        [ctx id]
         #:expand     [ex (replace id)])
  (make-infix #:precedence prec #:expand ex
              (make-rename-operator-proc #:id     id #:get get
                                         #:wrap-e we #:ctx ctx)))

(define (make-rename-operator-proc
         #:id         id
         #:get        get
         #:wrap-e     [we #f]
         #:ctx        [ctx id])
  (define next (next-e* id ctx we))
  (λ(e stx)
    (define-values (es* stx*)(get (drop-token stx)))
    (values (next e es*) stx*)))


(define (next-e* id ctx we)
  (define filter-args
    (if we (λ(l r)(cons (datum->syntax ctx (list-wrap l))
                        (list-wrap r)))
           (λ(l r)(let ([rs (list-wrap r)])
                    (if l (cons l rs) rs)))))
  (λ(l r)(datum->syntax ctx (cons id (filter-args l r)))))


(define ((replace id) stx)
  (let ([stx-e (syntax-e stx)])
    (cond[(list? stx-e) (datum->syntax stx (cons id (cdr stx-e)))]
         [else id])))


(provide
 prop:precedence
 prec-cmp token-prec
  
 drop-token get-cmp
 get-< get-<=
 get-none get-first
 
 make-infix
 make-operator-proc make-rename-operator-proc
 make-operator make-rename-operator)
