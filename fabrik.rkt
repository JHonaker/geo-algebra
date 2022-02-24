#lang racket

(require "geometric-algebra.rkt")
(require "conformal.rkt")

(struct joint (loc next-joint prev-joint d-to-next d-to-prev) #:mutable)
;; A Joint is a structure
;;   (struct ConformalPoint Joint Joint Number Number)
;; interpretation: a joint is the location of a point of movement in a kinematic chain
;; it has a reference to the next and previous joints in a chain
;; as well as the distance to each

;; Multivector -> Joint
;; creates an isolated joint with no connections
(define (make-iso-joint loc)
  (joint loc #f #f #f #f))

;; Joint Joint -> Void
;; effect: links a joint from to a joint to
;; sets the distances as well
(define (link-joints! from to)
  (let ([dist (point-dist (joint-loc from) (joint-loc to))])
    (set-joint-next-joint! from to)
    (set-joint-prev-joint! to from)
    (set-joint-d-to-next! from dist)
    (set-joint-d-to-prev! to dist)))

;; Joint -> Bool
;; determines if a joint is a tip
;; i.e. no previous joint
(define (is-tip? j)
  (false? (joint-prev-joint j)))

;; Joint -> Bool
;; determines if a joint is a tail
;; i.e. no next joint
(define (is-tail? j)
  (false? (joint-next-joint j)))

;; Joint -> Multivector
;; creates the dual sphere for the projection of next jont
(define (sphere-next j)
  (let ([center (joint-loc j)]
        [r (joint-d-to-next j)])
    (dual-sphere/center center r)))

;; Joint -> Multivector
;; creates the dual sphere for the projection of previous jont
(define (sphere-prev j)
  (let ([center (joint-loc j)]
        [r (joint-d-to-prev j)])
    (dual-sphere/center center r)))

;; Joint -> Void
;; effect: projects and moves previous joint to sphere centered on joint J
(define (project-prev-joint! j)
  (let ([S (sphere-prev j)]
        [prev (joint-prev-joint j)])
    (set-joint-loc! prev (project-point->sphere (joint-loc prev) S))))

;; Joint -> Void
;; effect: projects and moves next joint to sphere centered on joint J
(define (project-next-joint! j)
  (let ([S (sphere-next j)]
        [next (joint-next-joint j)])
    (set-joint-loc! next (project-point->sphere (joint-loc next) S))))

;; Joint [Joint -> Maybe Joint] -> Void
;; effect: projects joints in the the kinematic chain in sequence to resatisfy constraints
(define (project-chain! tip #:next next-fn #:project project-next-fn!)
  (when (next-fn tip)
    (project-next-fn! tip)
    (project-chain! (next-fn tip) #:next next-fn #:project project-next-fn!)))

;; Joint Multivector -> Void
;; effect: sets tip's location to target and projects chain
(define (project-chain-forward! tip target)
  (set-joint-loc! tip target)
  (project-chain! tip #:next joint-next-joint #:project project-next-joint!))

;; Joint Multivector -> Void
;; effect: sets tails's location to target and projects chain
(define (project-chain-backward! tail target)
  (set-joint-loc! tail target)
  (project-chain! tail #:next joint-prev-joint #:project project-prev-joint!))


;;; Chains

(struct chain tip tail)
;; A Chain is a structure
;;   (struct Joint Joint)
;; 
