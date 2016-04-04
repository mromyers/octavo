#lang racket/base
(require syntax/parse)

(define-values (prop:token token? token-proc)
  (make-struct-type-property 'token))

(define-values (prop:tag-token tag-token? tag-token-ref)
  (make-struct-type-property 'tag-token))

(define (id-val t)
  (if (identifier? t)
      (syntax-local-value t (λ()#f))
      #f))

(define (add-tag stx)
  (let ([t-sym (case (syntax-property stx 'paren-shape)
                 [(#f) '#%parens]
                 [(#\[) '#%brackets]
                 [(#\{) '#%braces])])
    (with-syntax ([t (datum->syntax stx t-sym)]
                  [(stuff ...) stx])
      #'(t stuff ...))))

(define (do-parse e tok stx dom?)
  (if tok
      ((token-proc tok) tok e stx dom?)
      (with-syntax ([(t₀ t₁ ...) stx])
        (if e (values e stx)
            (parse #'t₀ #'(t₁ ...) dom?)))))

(define (parse e stx dom?)
  (syntax-parse stx
    [((~and (t₀-tag stuff ...) t₀) t₁ ...)
     (let ([t₀-tag-val (id-val #'t₀-tag)])
       (if (tag-token? t₀-tag-val)
           (if (and (dom? #'t₀) e) (values e stx)
               (do-parse e (and (token? t₀-tag-val) t₀-tag-val) stx dom?))
           (with-syntax ([t₀* (add-tag #'t₀)])
             (parse e #'(t₀* t₁ ...) dom?))))]
    [((~and () t₀) t₁ ...)
     (with-syntax ([t₀* (add-tag #'t₀)])
       (parse e #'(t₀* t₁ ...) dom?))]
    [(t₀ t₁ ...)
     (if (and (dom? #'t₀) e) (values e stx)
         (let ([t₀-val (id-val #'t₀)])
           (do-parse e (and (token? t₀-val) t₀-val) stx dom?)))]
    [() (values e stx)]))

(define (parse-all stx)
  (let-values ([(e* stx*)(parse #f stx (λ(t) #f))]) e*))



(define (split-next s lst stx)
  (let loop ([lst-s '()][lst-e lst])
    (cond [(null? lst-e)
           (values
            (datum->syntax stx (reverse lst-s)) lst-e)]
          [(equal? s (syntax->datum (car lst-e)))
           (values
            (datum->syntax stx (reverse lst-s)) lst-e)]
          [else
           (loop (cons (car lst-e) lst-s) (cdr lst-e))])))

(define (keyword-split s stx)
  (let ([lst (syntax-e stx)])
    (if (null? lst) lst
        (let-values ([(x rst)(split-next s lst stx)])
          (if (null? rst) (list x)
              (let loop ([xs (list x)][lst rst])
                (let-values ([(x rst)(split-next s (cdr lst) stx)])
                  (if (null? rst) (reverse (cons x xs))
                      (loop (cons x xs) rst)))))))))


(provide prop:token prop:tag-token
         parse parse-all keyword-split)
