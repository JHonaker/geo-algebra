#lang racket

(provide (all-defined-out))

(define (list-updatef lst pred val)
  (cond [(empty? lst) (cons val empty)]
        [(pred (first lst)) (cons val (rest lst))]
        [else (cons (first lst) (list-updatef (rest lst) pred val))]))

(define (all-sequences-of arity zero one)
  (map (lambda (index)
         (index->choices index arity zero one))
       (range (expt 2 arity))))

(define (index->choices index arity zero one)
  (let loop ((i 0) (index index) (choices '()))
    (if (< i arity)
        (loop (+ i 1)
              (quotient index 2)
              (cons (if (odd? index) one zero)
                    choices))
        choices)))

(define-syntax-rule (map/method obj meth xs)
  (map (λ (x) (send obj meth x)) xs))

(define (approx-= x y [tol 1e-10])
  (< (abs (- x y)) tol))

(define (approx-zero? x [tol 1e-10])
  (approx-= x 0 tol))

(define (approx-one? x [tol 1e-10])
  (approx-= x 1 tol))
