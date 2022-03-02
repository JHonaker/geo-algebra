#lang racket


(require "../generic-arith.rkt"
         "algebra.rkt"
         "geometric-algebra.rkt"
         "dual-algebra.rkt")



(define-geo-algebra conformal-algebra C: [p 4] [q 1] [r 0])

(define-dual-algebra dual-algebra D: [n-dual 3])

(define-geo-algebra diff-conf-algebra dC: [p 4] [q 1] [r 3] [base-field dual-algebra])

(define (d: x) (dC: ((D: 1 d 1) 1 2) ((D: 1 d 2) 0 1)))


(geo (C: (1.0 1) (1.0 2)) (C: (1.0 1) (1.0 3)))
(geo (d: 0) (mul (d: 0) (d: 0)))



(define (r [x 1])
  (* (- (* (random) 2) 1) x))

(require "taylor-approx.rkt")

(define (rotate R x)
  (geo* (exp/taylor (scale R -0.5) #:max-order 25)
        x
        (exp/taylor (scale R 0.5) #:max-order 25)))

(define-geo-algebra r3 r3: [p 3] [q 0] [r 0] [base-field dual-algebra])

(define (random-point [x 1])
  (r3: ((D: (r x)) 0) ((D: (r x)) 1) ((D: (r x)) 2)))

(define (random-points n)
  (build-list n (λ _ (random-point))))

(define (random-bivector)
  (r3: ((D: (r)) 0 1) ((D: (r)) 0 2) ((D: (r)) 1 2)))

(define num-pts 20)
(define pts (random-points num-pts))
(define true-bv (random-bivector))
(define pts-rot (map (λ (p) (add p (random-point 0.01)))
                     (map (curry rotate true-bv) pts)))

(define guess (r3: ((D: d 0) 0 1) ((D: d 1) 0 2) ((D: d 2) 1 2)))

(define (cost G Pa Pb)
  (let loop ([total (zero-like (first Pa))] [Pas Pa] [Pbs Pb])
    (if (empty? Pas)
        (element->base (div total (length Pa)))
        (loop (add total (sqnorm (sub (rotate G (first Pas)) (first Pbs))))
              (rest Pas)
              (rest Pbs)))))

(define (get-gradient x)
  (send (algebra-of x) select-grade x 1))

(define (update-guess guess [eps 0.1])
  (define c (cost guess pts pts-rot))
  (define grad (get-gradient c))
  (define grad-coefs ((el-ref grad) '(0) '(1) '(2)))
  (define bivector-grad
    (match-let ([(list x01 x02 x12) grad-coefs])
      (r3: (x01 0 1) (x02 0 2) (x12 1 2))))
  (sub guess (mul eps bivector-grad)))

(define-values (fit-bivector path)
  (for/fold ([g guess]
             [guesses (list guess)]
             #:result (values g (reverse guesses)))
            ([_ (in-range 100)])
    (let ([new-guess (update-guess g)])
      (values new-guess (cons new-guess guesses)))))

(define (->point x)
  (map (λ (p) ((el-ref p) '()))
       ((el-ref x) '(0) '(1) '(2))))

(define (pt-path Rs p)
  (for/fold ([ps (list p)]
             #:result ps)
            ([R (in-list Rs)])
    (cons (rotate R p) ps)))

(require (except-in plot inverse))

(define (convert-point x)
  ((el-ref x) '(0) '(1) '(2)))

(define (convert-coef x)
  (append-map (λ (y) ((el-ref y) '())) x))

(define (convert-path x)
  (map (compose convert-coef convert-point) x))

(define paths (map (λ (p) (pt-path path p)) pts))
(define lines (map convert-path paths))

(define (pt->point pt)
  (append-map (λ (y) ((el-ref y) '())) ((el-ref pt) '(0) '(1) '(2))))


(plot3d (list* (points3d (map pt->point pts))
               (points3d (map pt->point pts-rot) #:color "blue")
               (map lines3d lines))
        #:altitude 10)
