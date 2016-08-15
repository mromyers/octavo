#lang racket
(require (for-meta 2 racket/base
                   racket/syntax
                   syntax/parse)
         (for-syntax racket/syntax
                     racket/pretty
                     syntax/parse
                     "token.rkt"
                     "parse.rkt"
                     "infix.rkt"))

(begin-for-syntax
  (define-syntax (token stx)
    (define-values (is-unary? infix-proc)  (values #f #f))
    
    (define-syntax-class normal-opt
      [pattern (~and kwd (~or #:is-tag #:expand))
               #:with standard #'kwd]
      [pattern (~or #:precedence #:prec)
               #:with standard #'#:precedence]
      [pattern (~or #:statement-procedure #:stmt-proc)
               #:with standard #'#:stmt-proc])
    
    (define-splicing-syntax-class opt+val
      [pattern (~seq k:normal-opt v)
               #:with (result ...) #'(k.standard v)]
      [pattern (~seq (~or #:infix-procedure #:infix-proc) proc)
               #:do [(set! infix-proc #'proc)]
               #:with (result ...) #'()]
      [pattern (~seq #:is-prefix-unary b)
               #:do [(set! is-unary? (syntax-e #'b))]
               #:with (result ...) #'()])
    (syntax-parse stx
      [(_ o+v:opt+val ...)
       (if infix-proc
           (with-syntax* ([infix-proc infix-proc]
                          [infix-proc* (if is-unary?
                                           #'(make-unary-proc infix-proc)
                                           #'infix-proc)])
             #'(make-token #:infix-proc infix-proc*
                           o+v.result ... ...))
           #'(make-token o+v.result ... ...))]))

  (define-syntax-class token-opt
    [pattern (~or #:is-tag
                  #:is-prefix-unary
                  #:precedence #:prec
                  #:infix-procedure #:infix-proc
                  #:statement-procedure #:stmt-proc
                  #:expand)])

  (define-syntax-class not-token-opt
    [pattern (~not a:token-opt)])

  (define-splicing-syntax-class token-opt+val
    [pattern (~seq a:token-opt v)
             #:with (splice ...) #'(a v)])

  (define (split-opt-body stx)
    (syntax-parse stx
      [(o+v₁:token-opt+val ...
        body:not-token-opt ...
        o+v₂:token-opt+val ...)
       (values #'(o+v₁.splice ... ... o+v₂.splice ... ...)
               #'(body ...))]))

)
(provide define-token)
(define-syntax (define-token stx)
  (syntax-parse stx
    [(_ name:id stuff ...)
     (define-values (opts body)(split-opt-body #'(stuff ...)))
     (with-syntax ([(o+v ...) opts])
       (syntax-parse body
         [() #'(define-syntax name (token o+v ...))]
         [(x ...) #'(define-syntax name
                      (let() x ... (token o+v ...)))]))]))

(define-syntax (test-split stx)
  (syntax-parse stx
    [(_ stuff ...)
     (define-values (opts body)(split-opt-body #'(stuff ...)))
     (printf "Options:\n")
     (pretty-print (syntax->datum opts))
     (printf "Body:\n")
     (pretty-print (syntax->datum body))
     #'(void)]))

(provide define-infix)
(define-syntax (define-infix stx)
  (syntax-parse stx
    [(_ name:id stuff ...)
     (define-values (opts body)(split-opt-body #'(stuff ...)))
     (with-syntax ([(o+v ...) opts][(body ...) body])
       #'(define-syntax name (token #:infix-proc body ... o+v ...)))]
    [(_ (name:id stx*) stuff ...)
     (define-values (opts body)(split-opt-body #'(stuff ...)))
     (with-syntax ([(o+v ...) opts][(body ...) body])
       #'(define-syntax name
           (token #:is-prefix-unary #t
                  #:infix-proc (λ(stx*) body ...) o+v ...)))]
    [(_ (name:id e* stx*) stuff ...)
     (define-values (opts body)(split-opt-body #'(stuff ...)))
     (with-syntax ([(o+v ...) opts][(body ...) body])
       #'(define-syntax name
           (token #:is-prefix-unary #f
                  #:infix-proc (λ(e* stx*) body ...) o+v ...)))]))

(define-syntax-rule (define-tag-infix blob stuff ...)
  (define-infix blob #:is-tag #t stuff ...))

(provide define-statement)
(define-syntax (define-statement stx)
  (syntax-parse stx
    [(_ name:id stuff ...)
     (define-values (opts body)(split-opt-body #'(stuff ...)))
     (with-syntax ([(o+v ...) opts][(body ...) body])
       #'(define-syntax name (token #:stmt-proc body ... o+v ...)))]
    [(_ (name:id args ...) stuff ...)
     (define-values (opts body)(split-opt-body #'(stuff ...)))
     (with-syntax ([(o+v ...) opts][(body ...) body])
       #'(define-syntax name
           (token #:stmt-proc (λ(args ...) body ...) o+v ...)))]))

#;
(define-syntax (define-infix stx)
  (define is-unary? #f)
  (define-syntax-class infix-option+value
    [pattern (~and kwd (~or #:is-tag #:expand))
             #:with standard #'kwd]
    [pattern (~or #:precedence #:prec)
             #:with standard #'#:precedence]
    [pattern (~or #:statement-procedure #:stmt-proc)
             #:with standard #'#:stmt-proc])
  
  (define-splicing-syntax-class infix-opt
    [pattern (~seq k:infix-option+value v)
             #:with (result ...) #'(k.result v)]
    [pattern (~seq #:is-prefix-unary b)
             #:do [(set! is-unary? (syntax-e #'b))]
             #:with (result ...) #'()])

  (define (make-def name proc opts)
    (with-syntax ([name name][proc proc][(o ...) opts])
      (cond [is-unary? #'(define-syntax name
                           (make-token #:infix-proc
                                       (make-unary-proc proc) o ...))]
            [else #'(define-syntax name
                      (make-token #:infix-proc proc o ...))])))
  
  (syntax-parse stx
    [(_ name:id o:infix-opt ...)
     #'(define-syntax name (make-token o.result ... ...))]
    [(_ name:id o₁:infix-opt ... proc o₂:infix-opt ...)
     (make-def #'name #'proc
               #'(o₁.result ... ... o₂.result ... ...))]
    [(_ (name:id stx*)
        o₁:infix-opt ... body:not-infix-opt ... o₂:infix-opt ...)
     (set! is-unary? #t)
     (make-def #'name #'(λ(stx*) body ...)
               #'(o₁.result ... ... o₂.result ... ...))]
    [(_ (name:id e* stx*)
        o₁:infix-opt ... body:not-infix-opt ... o₂:infix-opt ...)
     (set! is-unary? #f)
     (make-def #'name #'(λ(e* stx*) body ...)
               #'(o₁.result ... ... o₂.result ... ...))]))


(provide define-syntax/operator)
(define-syntax (define-syntax/operator stx)
  (define-values (association precedence) (values #f #f))
  (define-splicing-syntax-class op-opt
    [pattern (~seq #:tag)
             #:with (result ...) #'(#:tag #t)]
    [pattern (~seq (~or #:prec #:precedence) prec)
             #:do [(set! precedence #'prec)]
             #:with (result ...) #'(#:precedence prec)]
    [pattern (~seq (~and dkt (~or #:drop-token #:keep-token)))
             #:with (result ...)
             (syntax-parse #'dkt
               [#:drop-token #'(#:drop-token #t)]
               [#:keep-token #'(#:drop-token #f)])]
    [pattern (~seq (~or #:com #:combine) com)
             #:with (result ...) #'(#:combine com)]
    [pattern (~seq #:get get)
             #:with (result ...) #'(#:get get)]
    [pattern (~seq (~or #:assoc #:association) assoc)
             #:do [(set! association (syntax->datum #'assoc))]
             #:with (result ...) #'()]
    [pattern (~seq #:expand ex)
             #:with (result ...) #'(#:expand ex)])
  (syntax-parse stx
    [(_ name:id o:op-opt ...)
     (if association
         (with-syntax ([get (with-syntax ([prec precedence])
                              (case association
                                [('left)  #'(get-<= prec)]
                                [('right) #'(get-<  prec)]))])
           #'(define-syntax name (make-operator #:get get o.result ... ...)))
         #'(define-syntax name (make-operator o.result ... ...)))]))

(provide define-rename-operator)
(define-syntax (define-rename-operator stx)
  (define-values (association precedence) (values #f #f))
  (define-splicing-syntax-class op-opt
    [pattern (~seq (~or #:prec #:precedence) prec)
             #:do [(set! precedence #'prec)]
             #:with (result ...) #'(#:precedence prec)]
    [pattern (~seq #:id o)
             #:with (result ...) #'(#:id #'o)]
    [pattern (~seq (~or #:wrap-e #:wrap-l #:wrap-left))
             #:with (result ...) #'(#:wrap-e #t)]
    [pattern (~seq #:get get)
             #:with (result ...) #'(#:get get)]
    [pattern (~seq (~or #:assoc #:association) assoc)
             #:do [(set! association (syntax->datum #'assoc))]
             #:with (result ...) #'()]
    [pattern (~seq #:expand ex)
             #:with (result ...) #'(#:expand ex)])
  (syntax-parse stx
    [(_ [name:id o:id] stuff ...)
     #'(define-rename-operator name #:id o stuff ...)]
    [(_ name:id o:id stuff ...)
     #'(define-rename-operator name #:id o stuff ...)]
    [(_ name:id opt:op-opt ...)
     (if association
         (with-syntax ([get (with-syntax ([prec precedence])
                              (case association
                                [('left)  #'(get-<= prec)]
                                [('right) #'(get-<  prec)]))])
           #'(define-syntax name (make-rename-operator
                                  #:get get opt.result ... ...)))
         #'(define-syntax name (make-rename-operator
                                opt.result ... ...)))]))

(provide define-operator)
(define-syntax (define-operator stx)
  (define-splicing-syntax-class op-opt
    [pattern (~seq (~or #:prec #:precedence) prec)
             #:with (result ...) #'(#:precedence prec)]
    [pattern (~seq #:get get)
             #:with (result ...) #'(#:get get)]
    [pattern (~seq (~or #:assoc #:association) assoc)
             #:with (result ...) #'(#:assoc assoc)])
  (syntax-parse stx
    [(_ name:id opt₁:op-opt ... fun:id opt₂:op-opt ...)
     #'(define-rename-operator name #:id fun
         opt₁.result ... ... opt₂.result ... ...)]
    [(_ name:id opt₁:op-opt ... body opt₂:op-opt ...)
     #'(begin
         (define fun body)
         (define-rename-operator name #:id fun
           opt₁.result ... ... opt₂.result ... ...))]
    [(_ (name:id args ...) opt:op-opt ... body ...)
     (with-syntax ([fun (format-id (generate-temporary) "~a₀" #'name)])
     #'(begin
         (define (fun args ...)
           body ...)
         (define-rename-operator name #:id fun
           opt.result ... ...)))]))

(provide #%parse)
(define-syntax (#%parse stx)
  (parse-all (drop-token stx)))
