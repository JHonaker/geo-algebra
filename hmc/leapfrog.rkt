#lang racket

; the leapfrog integrator used in HMC sampling

(provide leapfrog)

(require "../generic-arith.rkt"
         math/flonum)

;; leapfrog : Parameter Momentum StepSize Function -> Parameter Momentum
;; the leapfrog integrator simulated hamiltonian dynamics of a particle
(define (leapfrog param momentum step-size grad-fn)
  (define (half-step param momentum)
    (flvector+ momentum (flvector-scale (grad-fn param) (fl/ step-size 2.0))))
  (let* ([half-step-momentum (half-step param momentum)]
         [full-step-param (flvector+ param (flvector-scale half-step-momentum step-size))]
         [full-step-momentum (half-step full-step-param half-step-momentum)])
    (values full-step-param
            full-step-momentum)))
