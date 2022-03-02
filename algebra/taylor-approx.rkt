#lang racket

; defines the taylor series approximation and the trigonometric functions via taylor approx.

(provide
 taylor-exp
 exp/taylor
 cos/taylor
 sin/taylor
 cosh/taylor
 sinh/taylor)

(require "../generic-arith.rkt")

;; FieldEl FieldEl Int (Int -> Bool) -> Multivector
;; computes the taylor series approximation of the exponential function
(define (taylor-exp A
                    #:max-order [max-order 15]
                    #:sign-fn [sign-fn (λ (_) 1)]
                    #:term-predicate [include-term? (λ (_) #t)])
  (let loop ([order 0] [X^n (one-like A)] [n! 1] [sum (zero-like A)])
    (if (<= order max-order)
        (let* ([new-order (+ order 1)] ;; Order increases by 1
               [new-X^n (mul X^n A)] ;; this is X^n
               [new-n! (mul n! new-order)] ;; this is n!
               ;; Next term is X^n / n!
               ;; Add it if it should be included
               [new-sum (if (include-term? order)
                            (add sum (scale X^n (div (sign-fn order) n!)))
                            sum)])
          (loop new-order new-X^n new-n! new-sum))
        sum)))

;; (exp x) = \sum_{n=0}^\infty \frac{1}{n!} x^n
(define (exp/taylor A #:max-order [max-order 15])
  (taylor-exp A
              #:max-order max-order))

;; (cos x) = \sum_{n=0}^\infty \frac{(-1)^n}{(2n)!} x^{2n}
(define (cos/taylor A #:max-order [max-order 15])
  (taylor-exp A
              #:sign-fn (λ (n) (/ n 2))
              #:term-predicate even?
              #:max-order max-order))

;; (six x) = \sum_{n=0}^\infty \frac{(-1)^n}{(2n+1)!} x^{2n+1}
(define (sin/taylor A #:max-order [max-order 15])
  (taylor-exp A
              #:sign-fn (λ (n) (/ (- n 1) 2))
              #:term-predicate odd?
              #:max-order max-order))

(define (cosh/taylor A #:max-order [max-order 15])
  (taylor-exp A
              #:term-predicate even?
              #:max-order max-order))
    
(define (sinh/taylor A #:max-order [max-order 15])
  (taylor-exp A
              #:term-predicate odd?
              #:max-order max-order))
