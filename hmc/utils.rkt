#lang racket

(provide (all-defined-out))

(require math/flonum)

(define (flvector-sqnorm x)
  (flvector-sum (flvector-sqr x)))

;; draw-momentum : Int -> FlVector
;; draws a new momentum vector
(define (draw-momentum n)
  (flnormal-sample 0.0 1.0 n))

(define (total-energy log-lik-fn theta momentum)
  (define potential-energy (log-lik-fn theta))
  (define kinetic-energy (fl* -0.5 (flvector-sqnorm momentum)) )
  (fl+ potential-energy
       kinetic-energy))
