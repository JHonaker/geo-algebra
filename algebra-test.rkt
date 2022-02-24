#lang racket


(require "generic-arith.rkt")
(require "algebra.rkt")

(define-algebra conformal-algebra C: geometric-algebra% [p 4] [q 1] [r 0])

(define-algebra dual-algebra D: geometric-algebra% [p 0] [q 0] [r 3])

(define-algebra diff-conf-algebra dC: geometric-algebra% [p 4] [q 1] [r 3] [base-field dual-algebra])

(define (dual: x) (D: (x) (1 0) (1 1) (1 2)))

(define (d: x) (dC: ((dual: x) 1 2) ((dual: x) 0 1)))


(geo (C: (1.0 1) (1.0 2)) (C: (1.0 1) (1.0 3)))
(geo (d: 3) (mul (d: 1) 3))


;; (define (r)
;;   (- (* (random) 2) 1))

;; (define (rotate R x)
;;   (geo* (g:exp (scale R -0.5))
;;         x
;;         (g:exp (scale R 0.5))))

;; (define (cost G Pa Pb)
;;   (let loop ([total (zero-like (first Pa))] [Pas Pa] [Pbs Pb])
;;     (if (empty? Pa)
;;         ;; TODO when converting from number, we need to convert it to the base field first!
;;         (div total (length Pa))
;;         (add total (sqnorm (sub (rotate G (first Pas)) (first Pbs)))))))
