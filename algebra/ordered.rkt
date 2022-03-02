#lang racket

; defines an interface for an algebra with ordered elements

(provide ordered<%>

         g:=
         g:<
         g:>
         g:<=
         g:>=

         install-ordered-operations!
)

(require "../generic.rkt")
(require "algebra.rkt")

(define ordered<%>
  (interface (algebra<%>)
    o:<
    o:>))

(define o:<-generic (generic ordered<%> o:<))
(define o:>-generic (generic ordered<%> o:>))

(define/generic (g:< x y))
(define/generic (g:> x y))
(define/generic (g:<= x y))
(define/generic (g:>= x y))

(define/implementation (g:< [x number?] [y number?]) (< x y))
(define/implementation (g:> [x number?] [y number?]) (> x y))
(define/implementation (g:<= [x number?] [y number?]) (<= x y))
(define/implementation (g:>= [x number?] [y number?]) (>= x y))

(define (install-ordered-operations! C)
  (let ([alg-element? (element-of? C)])
    (define/implementation (g:< [x alg-element?] [y alg-element?])
      (send-generic C o:<-generic x y))

    (define/implementation (g:> [x alg-element?] [y alg-element?])
      (send-generic C o:>-generic x y))

    (define/implementation (g:<= [x alg-element?] [y alg-element?])
      (or (send-generic C o:<-generic x y)
          (g:= x y)))
    
    (define/implementation (g:>= [x alg-element?] [y alg-element?])
      (or (send-generic C o:>-generic x y)
          (g:= x y)))))
