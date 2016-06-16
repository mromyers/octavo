#lang racket/base
(require "private/parse.rkt")

(provide
 ;; from parse.rkt
 parse parse-all
 prec-cmp
 drop-token get-cmp
 get-< get-<=
 get-none get-first
 ;; local
 )






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

