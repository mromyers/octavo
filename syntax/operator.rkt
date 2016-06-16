#lang racket/base
(require "../syntax.rkt"
         "token.rkt"
         racket/syntax
         syntax/parse)

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
  (define maybe-drop (if dt drop-token (λ(stx) stx)))
  (define com* (if cc (com-case com) com))
  (define (proc e stx)
    (define-values (e* stx*)(get (maybe-drop stx)))
    (values (com* e e*) stx*))
  (make-token proc #:tag tg #:precedence prec #:expand ex))

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

(define (make-rename-operator
         #:id         id
         #:get        get
         #:precedence prec
         #:wrap-e     [we #f]
         #:ctx        [ctx id]
         #:expand     [ex (replace id)])
  (define next (next-e* id ctx we))
  (define (proc e stx)
    (define-values (es* stx*)(get (drop-token stx)))
    (values (next e es*) stx*))
  (make-token proc #:precedence prec #:expand ex))

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
