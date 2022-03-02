#lang racket

; Defines the interface expected for an algebraic field

(provide field<%>

         zero-element
         one-element

         install-field-operations!
         install-just-field-operations!)

(require "../generic.rkt"
         "../generic-arith.rkt")
(require "algebra.rkt")

;; The algebraic structures we are interested are defined on elements of X with
;; the following interface
(define field<%>
  (interface (algebra<%>)
    ;; -> X
    ;; returns the zero-like additive identity of the algebra
    f:zero-element
    
    ;; -> X
    ;; returns the one-like multiplicative identity of the algebra
    f:one-element
    
    ;; Addition, Subtraction, Multiplication, and Division
    f:+ f:- f:* f:/

    ;; Additive and Multiplicative inverses
    f:+-inverse f:*-inverse

    ;; Scalar multiplication
    f:scale
    ))

(define f:zero-element-generic (generic field<%> f:zero-element))
(define f:one-element-generic (generic field<%> f:one-element))

(define f:+-generic (generic field<%> f:+))
(define f:--generic (generic field<%> f:-))
(define f:*-generic (generic field<%> f:*))
(define f:/-generic (generic field<%> f:/))
(define f:+-inverse-generic (generic field<%> f:+-inverse))
(define f:*-inverse-generic (generic field<%> f:*-inverse))
(define f:scale-generic (generic field<%> f:scale))

(define (zero-element alg)
  (send-generic alg f:zero-element-generic))

(define (one-element alg)
  (send-generic alg f:one-element-generic))

(define (install-just-field-operations! F)
  (define-binary-ops F
    (add f:+-generic)
    (sub f:--generic)
    (mul f:*-generic)
    (div f:/-generic))

  (define-unary-ops F
    (neg f:+-inverse-generic)
    (inverse f:*-inverse-generic))
  
  (let ([alg-element? (element-of? F)])
    (define/implementation (scale [x alg-element?] [c any-type?])
      (send-generic F f:scale-generic x c))
    
    (define/implementation (zero-like [x alg-element?])
      (send-generic F f:zero-element-generic))
    
    (define/implementation (one-like [x alg-element?])
      (send-generic F f:one-element-generic))

    (define/implementation (eq-zero? [x alg-element?])
      (equal? (zero-element F) x))

    (define/implementation (eq-one? [x alg-element?])
      (equal? (one-element F) x))

    ))

(define (install-field-operations! F)
  (install-algebra-operations! F)
  (install-just-field-operations! F))
