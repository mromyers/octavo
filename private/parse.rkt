#lang racket/base
(require racket/syntax syntax/parse syntax/stx
         racket/set racket/list
         "token.rkt")

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
            (let-values ([(e* stx*) (infix-app v e stx+)])
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
     (cond [(or (tag-infix-transformer? v) (is-intro-tag? a*))
            (values #'t v stx)]
           [else (head-tok+val (replace-stx-car stx (add-tag a)))])]
    [(x ...) (head-tok+val (replace-stx-car stx (add-tag a)))]
    [t:id
     (define a* (add-def-ctx a))
     (define v (syntax-local-value a* (λ() #f)))
     (values a* v stx)]
    [else (values a #f stx)]))

(provide tok-val)
(define (tok-val a)
  (syntax-parse a
    [(t:id z ...)
     (define a* (add-def-ctx #'t))
     (define v (syntax-local-value a* (λ() #f)))
     (cond [(or (tag-infix-transformer? v) (is-intro-tag? a*)) v]
           [else (id-val (add-def-ctx (get-tag a)))])]
    [(x ...) (id-val (add-def-ctx (get-tag a)))]
    [t:id (id-val (add-def-ctx a))]
    [else #f]))


;; scopes
(provide parse-def-ctx)
(define parse-def-ctx (make-parameter #f))

(define (add-def-ctx id)
  (define ctx (parse-def-ctx))
  (cond [ctx  (internal-definition-context-introduce ctx id 'add)]
        [else id]))

(define (add-tag a)
  (datum->syntax
   a (cons (get-tag a) (syntax-e a))))

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
(provide add-jx)
(define (infix-app v e stx)
  (if (infix-transformer? v)
      ((infix-transformer v) v e stx)
      (if e (values e (add-jx stx))
          (with-syntax ([(a x ...) stx])
            (values #'a #'(x ...))))))

(define (add-jx stx)
  (if (and (not (stx-null? stx))
           (equal? (syntax-e (stx-car stx)) '#%jx))
      (raise-syntax-error #f "#%jx undefined")
      (let* ([lst (syntax-e stx)]
             [jx (datum->syntax (car lst) '#%jx )])
        (datum->syntax stx (cons jx lst)))))


;; misc utility
(define (id-val t)
  (syntax-local-value t (λ()#f)))

(define (replace-stx-car stx a)
  (define rst (cdr (syntax-e stx)))
  (datum->syntax stx (cons a rst)))

(define (stx-cons a stx)
  (datum->syntax stx (cons a (syntax-e stx))))

;;
(provide parse-block get-statement)

(define (parse-block stx #:get-statement [get-stmt #f])  
  (if get-stmt
      (parameterize ([get-statement-param get-stmt])
        (parse-block* stx))
      (parse-block* stx)))
      
(define (parse-block* stx)
  (let loop ([stx stx])
    (cond [(stx-null? stx) '()]
          [else (let-values ([(stmt stx*)(get-statement stx)])
                  (cons stmt (loop stx*)))])))

(define (get-statement-default stx)
  (define val (tok-val (stx-car stx)))
  (cond [(statement-transformer? val)
         ((statement-transformer val) val stx)]
        [else 
         (define-values (fst rst)(split-when (syntax-e stx) is-sc?))
         (define rst* (if (null? rst) rst (cdr rst)))
         (define stmt
           (let ([t (datum->syntax stx '#%stmt)])
             (datum->syntax stx (cons t fst))))
         (values stmt (datum->syntax stx rst*))]))

(define get-statement-param (make-parameter get-statement-default))

(define (get-statement stx) ((get-statement-param) stx))

(define (split-when lst p?)
  (splitf-at lst (compose not p?)))

(define (is-sc? a)
  (equal? (syntax-e a) '\;))



