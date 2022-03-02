#lang racket

(provide (all-defined-out))

(require math/flonum
         math/distributions)

(define (->values x)
  (apply values x))

(define (flvector-dot x y)
  (flvector-sum (flvector* x y)))

(define (flvector-sqnorm x)
  (flvector-sum (flvector-sqr x)))

;; draw-momentum : Int -> FlVector
;; draws a new momentum vector
(define (draw-momentum n)
  (flnormal-sample 0.0 1.0 n))

(define (draw-momentum-for params)
  (draw-momentum (flvector-length params)))

;; total-energy : LogLik Parameter Momentum -> Flonum
;; computes the (log) total energy of a Hamiltonian system
;;
;; The Hamiltonian energy of a system is the:
;; (exp (+ potential-energy kinetic-energy))
;; where the potential energy is determined from the log-likelihood surface of some probability
;; distribution and the kinetic energy is determined from the random "kick" momentum
(define (total-energy log-lik-fn theta momentum)
  (define potential-energy (log-lik-fn theta))
  (define kinetic-energy (fl* -0.5 (flvector-sqnorm momentum)) )
  (fl+ potential-energy
       kinetic-energy))
