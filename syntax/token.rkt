#lang racket/base
(require "../private/parse.rkt")
(provide make-token
 (rename-out [prop:token      prop:token-procedure]
             [prop:tag-token  prop:token-is-tag]
             [prop:precedence prop:token-precedence]
             [token-proc      token-procedure]
             [token-prec      token-precedence]
             [tag-token?      token-is-tag?]))

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



