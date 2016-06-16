#lang racket/base
(require "parse.rkt"
         racket/syntax
         syntax/parse)

;; misc utility
(define (list-wrap e)(if e (list e) e))


;; token structs
(struct token-struct (proc ex)
  #:property prop:token
  (λ(self e stx)((token-struct-proc self) e stx))
  #:property prop:procedure (struct-field-index ex))
(struct tag-token-struct token-struct ()
  #:property prop:tag-token 0)

(struct infix-struct token-struct (prec)
  #:property prop:precedence (λ(self)(infix-struct-prec self)))
(struct tag-infix-struct infix-struct ()
  #:property prop:tag-token 0)

;; general token constructor
(provide make-token)
(define (make-token proc
               #:tag        [tg #f]
               #:precedence [prec #f]
               #:expand     [ex no-expand-error])
  (cond [prec (cond [tg   (tag-infix-struct proc ex prec)]
                    [else (infix-struct proc ex prec)])]
        [else (cond [tg   (tag-token-struct proc ex)]
                    [else (token-struct proc ex)])]))

(define (no-expand-error stx)
  (raise-syntax-error #f
    "There needs to be a more official sounding error message."))

;; operators
(provide make-operator make-rename-operator)

(define (make-operator #:combine     com
                       #:get         get
                       #:precedence  prec      
                       #:tag         [tg #f]
                       #:drop-token  [dt #t]
                       #:com-cases   [cc #t]
                       #:expand      [ex (com->ex com)])
  (define maybe-drop (if dt drop-token (λ(stx) stx)))
  (define com* (if cc (com-case com) com))
  (define (proc e stx)
    (define-values (e* stx*)(get (maybe-drop stx)))
    (values (com* e e*) stx*))
  (token proc #:tag tg #:precedence prec #:expand ex))

(define ((com-case com) l r)
  (cond[l    (cond[r    (com l r)]
                  [else (com l  )])]
       [else (cond[r    (com   r)]
                  [else (com    )])]))

(define ((com->ex com) stx)
  (syntax-parse stx
    [(_ (e₀) e₁) (com #'e₀ #'e₁)]
    [(_ ()   e₁) (com      #'e₁)]
    [(_ (e₀))    (com #'e₀     )]
    [(_ ())      (com          )]))

(define (make-rename-operator #:id         id
                              #:get        get
                              #:precedence prec
                              #:wrap-e     [we #f]
                              #:ctx        [ctx id]
                              #:expand     [ex (replace id)])
  (define next (next-e* id ctx we))
  (define (proc e stx)
    (define-values (es* stx*)(get (drop-token stx)))
    (values (next e es*) stx*))
  (token proc #:precedence prec #:expand ex))

(define (next-e* id ctx we)
  (define filter-args* (if we filter-args/wrap filter-args))
  (λ(l r)(datum->syntax ctx (cons id (filter-args* l r)))))

(define (filter-args      l r)
  (let ([rs (list-wrap r)])
    (if l (cons l rs) rs)))
(define (filter-args/wrap l r)
    (cons (datum->syntax ctx (list-wrap l)) (list-wrap r)))

(define ((replace id) stx)
  (let ([stx-e (syntax-e stx)])
    (cond[(list? stx-e) (datum->syntax stx (cons id (cdr stx-e)))]
         [else  id])))
