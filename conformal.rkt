#lang racket

(require "geometric-algebra.rkt")

(provide (all-defined-out))

;; The default export is conformal, because that's what I'm using

;; 0 1 2 are X Y Z
;; 3 = n_o, 4 = n_inf

;; Number Number Number -> Multivector
;; converts a euclidean point to a euclidean only multivector
(define (r3->euclid x y z)
  (mv: (b: x 0)
       (b: y 1)
       (b: z 2)))

;; Number Number Number -> Multivector
;; converts a euclidean point to a unit multivector
(define (unit-vector x y z)
  (let ([v (r3->euclid x y z)])
    (mv:scale v (mv:norm v))))

;; Multivector -> List-of Number
;; extracts the euclidean dimensions of a multivector
(define (extract-r3 mv)
  (define (blade-with-dim bs dim)
    (findf (λ (b) (equal? (blade-dims b) (list dim)))
           bs))
  (let* ([mv (select-grade mv 1)]
         [all-blades (multivector-blades mv)]
         [blades (map (λ (i) (blade-with-dim all-blades i)) '(0 1 2))])
    (map blade-coef (filter identity blades))))


(define b0 (mv: (b: 1.0 0)))
(define b1 (mv: (b: 1.0 1)))
(define b2 (mv: (b: 1.0 2)))
(define no (mv: (b: 1.0 3)))
(define ni (mv: (b: 1.0 4)))
(define no-ni (mv:wedge no ni))

;; Multivector -> Multivector
;; extracts a flat
(define (extract-flat p)
  (mv:/ (mv:contractl no-ni (mv:wedge no p))
        (mv:contractl no-ni p)))

;; Multivector -> Multivector
;; extracts the center of a dual sphere
(define (extract-center s)
  (let ([p (mv:sandwich ni s)])
    (mv:/ p (mv:contractl (mv:- ni) p))))

;;; Conformal Primitives

;; Number Number Number [Number] -> Multivector
;; returns the conformal point at x y z with weight w
(define (conf-point x y z [w 1.0])
  (let ([p (r3->euclid x y z)])
    (euclid->conf-point p w)))

;; Multivector -> Multivector
;; converts a Euclidean multivector to a conformal point
(define (euclid->conf-point p [w 1.0])
  (mv:scale (mv:+ no p (mv:scale (mv:geo* p p ni) 0.5))
            w))

;; Multivector -> Multivector
;; converts a conformal point to a Euclidean multivector
(define (conf-point->euclid p)
  (apply r3->euclid (extract-r3 p)))

;; Number Number Number Number -> Multivector
;; creates the dual plane whose normal vector points in x y z
;; that is located d distance from the origin
(define (dual-plane x y z d)
  (let ([normal-v (unit-vector x y z)])
    (mv:+ normal-v (mv:scale ni d))))

;; Number Number Number Number [Number] [Bool] -> Multivector
;; creates the dual sphere located at x y z with radius r
;; that is either real or imaginary
(define (dual-sphere/xyz x y z r [w 1.0] #:is-real [is-real #t])
  (let ([center (conf-point x y z)])
    (dual-sphere/center center r w #:is-real is-real)))

;; Multivector Number [Number] [Bool] -> Multivector
;; creates the dual sphere located at the conformal point center
;; with radius r that is either real or imaginary
(define (dual-sphere/center c r [w 1.0] #:is-real [is-real #t])
  (let ([real-sign (if is-real 1 -1)])
    (mv:scale (mv:+ c
                    (mv:scale ni
                              (* real-sign 0.5 (expt r 2))))
              w)))

;; Multivector -> (values Multivector Multivector)
;; decomposes a point pair into its constituent points
(define (decompose-pair pointpair)
  (let* ([pp-sq (mv:geo pointpair pointpair)]
         [pp (scalar->multivector (sqrt (multivector->scalar pp-sq)))]
         [ni-P (mv:contractl (mv:- ni) pointpair)]
         [p-plus (mv:/ (mv:- pointpair pp)
                       ni-P)]
         [p-minus (mv:/ (mv:+ pointpair pp)
                        ni-P)])
    (values p-plus p-minus)))

;; Multivector Multivector -> Multivector
;; returns the closest point in a point pair to a point P
(define (point-pair->closest-point pointpair P)
  (let-values ([(p+ p-) (decompose-pair pointpair)])
    (if (> (point-dist p+ P) (point-dist p- P))
        p-
        p+)))

;; Multivector Multivector -> Multivector
;; project the point P to the line L
(define (project-point->line P L)
  (let* ([ni-no (mv:wedge ni no)]
         [X (mv:contractl (mv:contractl P L) L)]
         [normalized-X (mv:/ X (mv:contractl (mv:- X) ni))]
         [euclid-X (mv:geo (mv:wedge X ni-no) ni-no)])
    (euclid->conf-point euclid-X)))

;; Multivector Multivector -> Number
;; computes the squared distance between two conformal points
(define (point-sq-dist a b)
  (let ([ip (multivector->scalar (mv:dot a b))])
    (* -2 ip)))

;; Multivector Multivector -> Number
;; computes the distance between two conformal points
(define (point-dist a b)
  (sqrt (point-sq-dist a b)))

;; Multivector Multivector -> Multivector
;; projects a conformal point P to a dual conformal sphere S
;; by finding the line L from P to the center of S,
;; intersecting L with S, and returning the closest
;; of the resulting point pair to P
(define (project-point->sphere P S)
  (let* ([C (extract-center S)]
         [L (mv:wedge* P C ni)]
         [intersection (mv:undual (mv:wedge (mv:dual L) S))])
    (point-pair->closest-point intersection P)))

;; Multivector Multivector -> Multivector
;; project a conformal point onto a direct plane
;; by reflecting a point in the plane
;; and finding the midpoint between the midpoint and the original point
(define (project-point->plane pt plane)
  (let* ([reflected-pt (mv:sandwich pt plane)]
         [mid-point (mv:scale (mv:+ (conf-point->euclid pt)
                                   (conf-point->euclid reflected-pt))
                             0.5)])
    mid-point))

;; Multivector Multivector -> Multivector
;; projects a point onto a direct conformal circle
;; by projecting the point onto the plane of the circle
;; then projecting that point onto the circle
(define (project-point->circle P C)
  (let* ([circle-plane (mv:wedge C ni)]
         [pt-on-plane (project-point->plane P circle-plane)]
         [circle-center (extract-center C)]
         [line-through-center (mv:wedge* pt-on-plane circle-center ni)]
         [intersection (mv:undual (mv:wedge (mv:dual line-through-center) (mv:dual C)))])
    (point-pair->closest-point intersection P)))

;; Multivector Multivector -> Multivector
;; reflect a direct flat in a dual plane
(define (reflect-flat-in-plane flat plane)
  (mv:sandwich (mv:involute flat) plane))
