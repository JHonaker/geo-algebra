#lang racket

(require "generic.rkt"
         "utils.rkt")

(provide
 zero-like
 one-like
 eq-zero?
 eq-one?
 add
 sub
 mul
 div
 ldiv
 scale
 inverse
 neg
 
 g:exp
 g:cos
 g:sin
 g:cosh
 g:sinh
 
 sqnorm
 norm

 add*
 sub*
 mul*
 )

(define/generic (zero-like x))
(define/generic (one-like x))
(define/generic (eq-zero? x))
(define/generic (eq-one? x))

(define/implementation (zero-like [x number?]) 0.0)
(define/implementation (one-like [x number?]) 1.0)
(define/implementation (eq-zero? [x number?]) (approx-zero? x))
(define/implementation (eq-one? [x number?]) (= x 1))

(define/generic (add x y))
(define/generic (sub x y))
(define/generic (mul x y))
(define/generic (div x y))
(define/generic (scale x y))
(define/generic (inverse x))
(define/generic (neg x))

(define/implementation (add [x number?] [y number?]) (+ x y))
(define/implementation (sub [x number?] [y number?]) (- x y))
(define/implementation (mul [x number?] [y number?]) (* x y))
(define/implementation (div [x number?] [y number?]) (/ x y))
(define/implementation (scale [x number?] [y number?]) (* x y))
(define/implementation (inverse [x number?]) (/ 1 x))
(define/implementation (neg [x number?]) (- x))

(define/generic (g:exp x))
(define/generic (g:cos x))
(define/generic (g:sin x))
(define/generic (g:cosh x))
(define/generic (g:sinh x))

(define/implementation (g:exp [x number?]) (exp x))
(define/implementation (g:cos [x number?]) (cos x))
(define/implementation (g:sin [x number?]) (sin x))
(define/implementation (g:cosh [x number?]) (cosh x))
(define/implementation (g:sinh [x number?]) (sinh x))

(define/generic (sqnorm x))
(define/generic (norm x))

(define/implementation (sqnorm [x number?]) (expt x 2))
(define/implementation (norm [x number?]) (abs x))

(define (add* . xs) (foldl add (zero-like (first xs)) xs))
(define (sub* . xs) (foldl sub (first xs) (rest xs)))
(define (mul* . xs) (foldl mul (one-like (first xs)) xs))

(define (ldiv x y) (mul (inverse x) y))
