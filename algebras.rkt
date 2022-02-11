#lang racket

(require "structs.rkt")
(require "metrics.rkt")

(provide (all-defined-out))

(define wedge-metric (make-diagonal-metric 0 0 5))

;;; Pseduoscalars

(struct pseudoscalar (val inv-val))
;; A Pseudoscalar is a struct
;;   (struct Multivector Multivector)
;; interpretation: the pseudoscalar represents the complete n-dimensional space
;; that defines the space dualization is relative to


(define conformal-ps (pseudoscalar (mv: (b: 1.0 0 1 2 3 4))
                                   (mv: (b: -1.0 0 1 2 3 4))))

(struct algebra (metric ps))
;; A GeometricAlgebra is a structure
;;   (struct ProductMetric Pseudoscalar)
;; interpretation: the algebraic structure for geometric algebra.
;; note: TODO this doesn't deal with dualization of degenerate metrics yet.

(define global-algebra (algebra (make-conformal-metric 3)
                               conformal-ps))

(define (p-scalar alg) (pseudoscalar-val (algebra-ps alg)))
(define (inv-p-scalar alg) (pseudoscalar-inv-val (algebra-ps alg)))

;; alg-product : Algebra (List-of Dim) (List-of Dim) -> Multivector
;; computes the product on basis blades associated with the algebra
(define (alg-product alg x y)
  (lookup-metric (algebra-metric alg) x y))
