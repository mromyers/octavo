#lang racket
(require (for-syntax racket/syntax
                     syntax/parse
                     "parse.rkt"
                     "infix.rkt"))

(provide define-infix)
(define-syntax (define-infix stx)
  (define-splicing-syntax-class tok-opt
    [pattern (~seq #:tag)
             #:with (result ...) #'(#:tag #t)]
    [pattern (~seq (~or #:prec #:precedence) prec)
             #:with (result ...) #'(#:precedence prec)]
    [pattern (~seq #:expand ex)
             #:with (result ...) #'(#:expand ex)])
  (syntax-parse stx
    [(_ name:id t₁:tok-opt ... proc t₂:tok-opt ...)
     #'(define-syntax name (make-infix proc t₁.result ... ... t₂.result ... ...))]
    [(_ (name:id args ...) t₁:tok-opt ... body ...)
     #'(define-syntax name
         (make-infix (λ(args ...) body ...)
                     t₁.result ... ...))]))

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


