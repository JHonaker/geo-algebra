#lang racket

; defines the dual number algebra in terms of the geometric algebra equivalent

(provide dual-algebra%
         install-dual-algebra-operations!
         define-dual-algebra)

(require "geometric-algebra.rkt"
         "real-numbers.rkt"
         "metric-defs.rkt"
         "blades.rkt"
         "ordered.rkt"
         (for-syntax syntax/parse))

(define dual-algebra%
  (class* geometric-algebra% (ordered<%>)
    (init [n-dual 1] [base (new real-numbers%)])
    (super-new [p 0] [q 0] [r n-dual] [base-field base])
    (inherit-field metric base-field)

    (set! metric (new dual-metric%
                      [dims n-dual]
                      [one-el 1]
                      [zero-el 0]
                      [element-fn make-blade]))
    
    
    (define/override (a:= x y)
      (let ([x-val (send this a:element->base (send this select-grade x 0))]
            [y-val (send this a:element->base (send this select-grade y 0))])
        (g:= x-val y-val)))

    (define/public (o:< x y)
      (let ([x-val (send this a:element->base (send this select-grade x 0))]
            [y-val (send this a:element->base (send this select-grade y 0))])
        (g:< x-val y-val)))

    (define/public (o:> x y)
      (let ([x-val (send this a:element->base (send this select-grade x 0))]
            [y-val (send this a:element->base (send this select-grade y 0))])
        (g:> x-val y-val)))))

(define (install-dual-algebra-operations! D)
  (install-geo-alg-operations! D)
  (install-ordered-operations! D))

(define-syntax (define-dual-algebra stx)
  (syntax-parse stx 
    [(_ name prefix args ...)
     #'(begin
         (define name (new dual-algebra% args ...))
         (install-dual-algebra-operations! name)
         (... (define-syntax (prefix -stx)
                (syntax-parse -stx #:datum-literals (d)
                  [(_ coef)
                   #'(send name a:make-element (list coef))]
                  [(_ d diff-dim)
                   #'(send name a:make-element (list 1 diff-dim))]
                  [(_ coef d diff-dim)
                   #'(send name a:make-element (list coef) (list 1 diff-dim))])
                )))]))
