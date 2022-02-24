#lang racket

(require "generic.rkt")

(provide
 zero-like
 one-like
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
 g:=
 g:<
 g:>
 g:<=
 g:>=
 )

(define/generic (zero-like x))
(define/generic (one-like x))

(define/implementation (zero-like [x number?]) 0.0)
(define/implementation (one-like [x number?]) 1.0)

(define/generic (add x y))
(define/generic (sub x y))
(define/generic (mul x y))
(define/generic (div x y))
(define/generic (ldiv x y))
(define/generic (scale x y))
(define/generic (inverse x))
(define/generic (neg x))

(define/implementation (add [x number?] [y number?]) (+ x y))
(define/implementation (sub [x number?] [y number?]) (- x y))
(define/implementation (mul [x number?] [y number?]) (* x y))
(define/implementation (div [x number?] [y number?]) (/ x y))
(define/implementation (ldiv [x number?] [y number?]) (/ y x))
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

(define/generic (g:= x y))
(define/generic (g:< x y))
(define/generic (g:> x y))
(define/generic (g:<= x y))
(define/generic (g:>= x y))

(define/implementation (g:= [x number?] [y number?]) (= x y))
(define/implementation (g:< [x number?] [y number?]) (< x y))
(define/implementation (g:> [x number?] [y number?]) (> x y))
(define/implementation (g:<= [x number?] [y number?]) (<= x y))
(define/implementation (g:>= [x number?] [y number?]) (>= x y))

