#lang racket

(require "nuts.rkt"
         math/flonum
         math/distributions)

(define (log-lik x)
  (flnormal-pdf 0.0 1.0 (flvector-ref x 0) #t))

(define (grad-fn x)
  (flvector (fl- (flvector-ref x 0))))

(define n 1000)
(define n-adapt 100)
(define samps-list (nuts (flvector 10.0) n n-adapt 0.65 log-lik grad-fn))

(define samps (first samps-list))
(define adapt-samps (second samps-list))

(define adapt-xs (map (λ (x) (flvector-ref x 0)) adapt-samps))
(define xs (map (λ (x) (flvector-ref x 0)) samps))
(/ (apply + xs) n)
(/ (apply + (map (λ (x) (expt x 2)) xs)) n)

(require plot)

(plot (list (lines (map list (range (add1 n-adapt)) (reverse adapt-xs)))
            (lines (map list (range (add1 n-adapt) (+ (add1 n-adapt) n)) (reverse xs))
                   #:color "blue")))

(plot (list
       (function (λ (x) (pdf (normal-dist 0.0 1.0) x)) -4 4)
       (density xs #:color "blue")))
