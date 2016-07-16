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
  ;; Pretend this reads as (parse [e #f] stx dom?)
  (case-lambda
    [(  stx dom?)(parse₀ #f stx dom?)]
    [(e stx dom?)(parse₀  e stx dom?)]))

(define (parse₀ e stx dom?)
  ;; Parse stx with e as the already parsed (left hand) expression, until
  ;; stx is empty, or the head token/value of stx satisfies dom?
  (if (stx-null? stx) (values e stx)
      (let-values ([(t v stx+)(head-tok+val stx)])
        (if (and e (dom? . [t v])) (values e stx)
            (let-values ([(e* stx*)((token-proc v) v e stx+)])
              (parse₀ e* stx* dom?))))))

(define parse-all
  ;; Pretend this reads as (parse-all [e #f] stx)
  (case-lambda
    [(  stx) (let-values ([(e* stx*) (parse   stx (λ(t v) #f))]) e*)]
    [(e stx) (let-values ([(e* stx*) (parse e stx (λ(t v) #f))]) e*)]))

(define (head-tok+val stx)
  ;; parsing front end - returns what will be used as the head token
  ;; its local value for parsing purposes. 
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
  (if e (values e (do-jx e stx))
      (with-syntax ([(a x ...) stx])
        (values #'a #'(x ...)))))
#;
(define (do-jx e stx)
  (with-syntax ([e e][(x ...) stx])
    (values #'(e x ...) #'())))

(define (do-jx e stx)
  (define (starts-with-jx? stx)
    (syntax-parse stx
      [() #f]
      [((~datum #%jx) x ...) #t]
      [else #f]))
  (if (starts-with-jx? stx)
      (raise-syntax-error #f "#%jx undefined")
      (let* ([lst (syntax-e stx)]
             [jx (datum->syntax (car lst) '#%jx )])
        (datum->syntax stx (cons jx lst)))))
