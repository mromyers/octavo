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
(provide prop:token prop:tag-token prop:precedence)

(define-values (prop:token token? token-ref)
  (make-struct-type-property 'token))
(define-values (prop:tag-token tag-token? tag-token-ref)
  (make-struct-type-property 'tag-token))
(define-values (prop:precedence has-prec? prec-ref)
  (make-struct-type-property 'precedence))

;; parsing
(provide parse parse-cmp parse-all)

(define (parse e stx dom?)
  (if (stx-null? stx) (values e stx)
      (let-values ([(t v stx+)(head-tok+val stx)])
        (if (and e (dom? t)) (values e stx)
            (let-values ([(e* stx*)((token-proc v) v e stx+)])
              (parse e* stx* dom?))))))


(define (parse-cmp e stx R m)
  ;; note: parse-cmp added only for efficiency.
  ;; (parse-cmp e stx R m) = (parse e stx (prec-cmp R m))
  (if (stx-null? stx) (values e stx)
      (let-values ([(t v stx+)(head-tok+val stx)])
        (if (and e (has-prec? v) (let ([n ((prec-ref v) v)]) (n . R . m)))
            (values e stx)
            (let-values ([(e* stx*)((token-proc v) v e stx+)])
              (parse-cmp e* stx* R m))))))

(define parse-all
  ;; note: parse-all added only for efficiency.
  ;; (parse-all [e #f] stx) = (parse e stx (λ(x) #f))
  (case-lambda 
    [(  stx)(-parse-all #f stx)]
    [(e stx)(-parse-all  e stx)]))
(define (-parse-all e stx)
  (if (stx-null? stx) e
      (let*-values ([(t v stx+)(head-tok+val stx)]
                    [(e* stx*) ((token-proc v) v e stx+)])
          (-parse-all e* stx*))))

(define parse-exp
  (case-lambda
    [(  stx)(parse #f stx (prec-cmp < 0))]
    [(e stx)(parse  e stx (prec-cmp < 0))]))

;; parsing front end
(define (head-tok+val stx)
  (define a (car (syntax-e stx)))
  (syntax-parse a
    [(t:id z ...)
     (define a* (add-def-ctx #'t))
     (define v (syntax-local-value a* (λ() #f)))
     (cond [(or (tag-token? v) (is-intro-tag? a*))
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

(define (add-tag stx)
  (define t-sym (case (syntax-property stx 'paren-shape)
                  [(#f) '#%parens][(#\[) '#%brackets][(#\{) '#%braces]))
  (datum->syntax stx (cons (datum->syntax stx t-sym)
                           (syntax-e stx))))

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
(provide token-proc token-prec tag-token? token-prec)
(define (token-proc v)
  (if (token? v) (token-ref v) e-fun))
 
(define (e-fun self e stx)
  (if e (do-jx e stx)
      (with-syntax ([(a x ...) stx])
        (values #'a #'(x ...)))))

(define (do-jx e stx)
  (with-syntax ([e e][(x ...) stx])
    (values #'(e x ...) #'())))

;; precedence
(provide prec-cmp)
(define (token-prec v)
  (if (has-prec? v) ((prec-ref v) v) #f))

(define ((prec-cmp R m) t)
  (let ([n (token-prec (id-val t))])
       (and n (n . R . m))))

;; 'get' functions
(provide drop-token get-cmp
         get-< get-<=
         get-none get-first)

(define (drop-token stx)
  (datum->syntax stx (cdr (syntax-e stx))))

(define ((get-cmp R m) stx)
  (parse-cmp #f stx R m))
(define (get-<  m) (get-cmp <  m))
(define (get-<= m) (get-cmp <= m))
(define ((get-none) stx) (values #f stx))
(define ((get-first) stx)
  (let ([stx-e (syntax-e stx)])
    (values (car stx-e) (datum->syntax stx (cdr stx-e)))))
