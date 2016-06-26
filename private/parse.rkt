#lang racket/base
(require racket/set
         racket/syntax
         syntax/parse
         syntax/stx)

;; misc utility
(define (id-val t)
  (syntax-local-value t (λ()#f)))

(define (replace-stx-car stx a)
  (define rst (cdr (syntax-e stx)))
  (datum->syntax stx (cons a rst)))


;; token properties
(provide prop:infix prop:tag-infix)

(define-values (prop:infix infix? infix-proc)
  (make-struct-type-property 'infix))
(define-values (prop:tag-infix tag-infix? tag-infix-ref)
  (make-struct-type-property 'tag-infix))

;; parsing
(provide parse parse-all)

(define parse
  (case-lambda
    [(  stx dom?)(parse₀ #f stx dom?)]
    [(e stx dom?)(parse₀  e stx dom?)]))

(define parse-all
  (case-lambda
    [(  stx)(parse-all₀ #f stx)]
    [(e stx)(parse-all₀  e stx)]))

(define (parse₀ e stx dom?)
  (if (stx-null? stx) (values e stx)
      (let-values ([(t v stx+)(head-tok+val stx)])
        (if (and e (dom? t v)) (values e stx)
            (let-values ([(e* stx*)((token-proc v) v e stx+)])
              (parse₀ e* stx* dom?))))))

(define (parse-all₀ e stx)
  (if (stx-null? stx) e
      (let*-values ([(t v stx+)(head-tok+val stx)]
                    [(e* stx*) ((token-proc v) v e stx+)])
          (parse-all₀ e* stx*))))

;; parsing front end
(define (head-tok+val stx)
  (define a (car (syntax-e stx)))
  (syntax-parse a
    [(t:id z ...)
     (define a* (add-def-ctx #'t))
     (define v (syntax-local-value a* (λ() #f)))
     (cond [(or (tag-infix? v) (is-intro-tag? a*))
            (values #'t v stx)]
           [else (head-tok+val (replace-stx-car stx (add-tag a)))])]
    [(x ...) (head-tok+val (replace-stx-car stx (add-tag a)))]
    [t:id
     (define a* (add-def-ctx a))
     (define v (syntax-local-value a* (λ() #f)))
     (values a* v stx)]
    [else (values a #f stx)]))

;; scopes
(provide parse-def-ctx)
(define parse-def-ctx (make-parameter #f))

(define (add-def-ctx id)
  (define ctx (parse-def-ctx))
  (cond [ctx  (internal-definition-context-introduce ctx id 'add)]
        [else id]))

(define (add-tag a)
  (datum->syntax a (cons (get-tag a) #;(datum->syntax stx t-sym)
                         (syntax-e a))))

(define (get-tag a)
  (datum->syntax
   a (case (syntax-property a 'paren-shape)
       [(#f)  '#%parens]
       [(#\[) '#%brackets]
       [(#\{) '#%braces])))

(define (is-intro-tag? t)
  (define intro-tags (set '#%parens '#%brackets '#%braces))
  (set-member? intro-tags (syntax->datum t)))

(provide with-parse-bindings)
(define-syntax-rule
  (with-parse-bindings [(x ...) expr]
    body ...)
  (parameterize ([parse-def-ctx (bind-parse-syntaxes (list x ...) expr)])
    body ...))

(define (bind-parse-syntaxes id-list expr)
  (define ctx (syntax-local-make-definition-context (parse-def-ctx) #f))
  (syntax-local-bind-syntaxes id-list expr ctx)
  ctx)


;; general token and defaults
(provide token-proc tag-infix?)
(define (token-proc v)
  (if (infix? v) (infix-proc v) e-fun))
 
(define (e-fun self e stx)
  (if e (do-jx e stx)
      (with-syntax ([(a x ...) stx])
        (values #'a #'(x ...)))))

(define (do-jx e stx)
  (with-syntax ([e e][(x ...) stx])
    (values #'(e x ...) #'())))
