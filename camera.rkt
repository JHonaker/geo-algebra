#lang racket

(require "geometric-algebra.rkt")
(require "conformal.rkt")

(struct camera (loc orientation focalpoint imageplane))
;; A Camera is a structure
;;   (struct ConformalPoint Rotor ConformalPoint DualPlane)
;; interpretation: a camera represents the structures necessary to produce a
;; perspective image in conformal geometric algebra

;; Number -> Camera
;; creates a camera with an image plane through the origin looking
;; down the positive Z-axis with a focal length of f
(define (make-camera f)
  (camera (conf-point 0.0 0.0 0.0)
          one-mv
          (conf-point 0.0 0.0 (- f))
          (dual-plane 0.0 0.0 1.0 0.0)))

;; Camera Multivector -> Camera
;; moves the camera according to the rotor
(define (move-camera cam rotor)
  (error 'move-camer "not implemented")
  cam)

;; Camera Multivector -> Multivector
;; constructs a line from the camera's focal point to the point P
(define (camera-line cam P)
  (mv:wedge* (focal-point cam) pt ni))

;; Camera Multivector -> Multivector
;; projects a point P onto the image plane, via perspective projection
(define (camera-project cam P)
  (let* ([L (camera-line cam P)]
         [plane (image-plane cam)]
         [ptpair (mv:wedge P (mv:dual L))]
         [flat-pt (extract-flat (mv:undual ptpair))])
    (euclid->conf-point flat-pt)))
