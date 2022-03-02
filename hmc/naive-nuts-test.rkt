#lang racket

(require "naive-nuts.rkt"
         math/flonum
         math/distributions)

(define (log-lik x)
  (flnormal-pdf 0.0 1.0 (flvector-ref x 0) #t))

(define (grad-fn x)
  (flvector (fl- (flvector-ref x 0))))

(define n 1000)
(define samps (naive-nuts (flvector 0.0) n 0.01 log-lik grad-fn))

(define xs (map (λ (x) (flvector-ref x 0)) samps))
(/ (apply + xs) n)
(/ (apply + (map (λ (x) (expt x 2)) xs)) n)
