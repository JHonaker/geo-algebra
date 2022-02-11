#lang racket

(provide (all-defined-out))

;; A Dim is a NonnegativeInteger
;; interpretation: the index of a spatial dimension, starting with 0

;;;; Blades

(define (blade-print blade port mode)
  (when mode (write-string "b[" port))
  (write (blade-coef blade) port)
  (write-string "b" port)
  (display (apply string-append (map number->string (blade-dims blade))) port)
  (when mode (write-string "]" port)))

(struct blade (coef dims grade)
  #:methods gen:custom-write
  [(define write-proc blade-print)])
;; A BasisBlade is a structure
;;  (struct Number List-of Dim Integer)
;; interpretation: a basis blade is one of the fundamental building blocks of
;; geometric algebra. It represents a weighted, oriented graded subspace of the
;; generating field.

(module+ test
  (define scalar-blade (blade 1.0 '() 0))
  (define g1-blade (blade 1.0 '(1) 1))
  (define g1-blade2 (blade 1.0 '(2) 1))
  (define g2-blade (blade 1.0 '(1 2) 2)))

;; Number (List-of Dim) -> Blade
;; creates a blade
(define (make-blade coef dims)
  (blade coef dims (length dims)))

;; Number Dim* -> Blade
;; a shorthand for creating blades
(define (b: c . dims)
  (make-blade c dims))

(define zero-blade (make-blade 0.0 '()))
(define one-blade (make-blade 1.0 '()))

;; Blade -> Multivector
;; converts a blade to a multivector
(define (blade->multivector b)
  (multivector (list b)))

;;;; Multivectors

(define (multivector-print mv port mode)
  (when mode (write-string "mv[" port))
  (let ([blades (multivector-blades mv)])
    (blade-print (first blades) port #f)
    (for ([b (in-list (rest blades))])
      (write-string " + " port)
      (blade-print b port #f)))
  (when mode (write-string "]" port)))

(struct multivector (blades)
  #:methods gen:custom-write
  [(define write-proc multivector-print)])
;; A Multivector is a structure
;;  (struct List-of BasisBlade)
;; where blades are ordered by grade and present dimensions
;; interpretation: A multivector represents the general structure of a mixed
;; grade object in geometric algebra.

(module+ test
  (define scalar-mv (multivector (list scalar-blade)))
  (define ex1-mv (multivector (list scalar-blade g1-blade)))
  (define ex2-mv (multivector (list scalar-blade g1-blade g2-blade))))

;; List-of Blade -> Multivector
;; creates a multivector with blades in the proper order
(define (make-multivector blades)
  (if (empty? blades)
      (multivector (list (make-blade 0.0 '())))
      (multivector (sort-blades (simplify-blades blades)))))

(define zero-mv (multivector (list zero-blade)))
(define one-mv (multivector (list one-blade)))

;; Multivector -> Bool
;; determines if a multivector is a scalar quantity
(define (mv:is-scalar? mv)
  (let ([blades (multivector-blades mv)])
    (and (empty? (rest blades)) ;; There is only one basis blade
         (empty? (blade-dims (first blades)))))) ;; and it has no dimensions

;; Blade+ -> Multivector
;; creates a multivector from a series of blades
(define (mv: . blades)
  (make-multivector blades))

;; Multivector -> Number
;; converts a scalar multivector to the number
(define (multivector->scalar mv)
  (if (mv:is-scalar? mv)
      (blade-coef (first (multivector-blades mv)))
      (error 'multivector->scalar "~a is not a scalar multivector" mv)))

;; Number -> Multivector
;; converts a scalar to a multivector
(define (scalar->multivector c)
  (multivector (list (blade c '()))))

;; Multivector -> Multivector
;; filters the grades of a multivector
(define (select-grade mv g)
  (multivector (filter (λ (b) (= g (blade-grade b)))
                       (multivector-blades mv))))

;; List-of Blade -> List-of Blade
;; sorts a list of blades by length and then by numberic value 
(define (sort-blades blades)
  (define (blade<? l r)
    (let ([l-grade (blade-grade l)]
          [r-grade (blade-grade r)])
      (cond [(= l-grade r-grade)
             (let loop ([l (blade-dims l)] [r (blade-dims r)])
               (cond [(empty? l) #f]
                     [(= (first l) (first r)) (loop (rest l) (rest r))]
                     [else (< (first l) (first r))]))]
            [(< l-grade r-grade) #t]
            [else #f])))
  (sort blades blade<?))

;; List-of Blade -> List-of Blade
;; simplifies a multivector by combining basis elements
(define (simplify-blades blades)
  ;; Blade Blade -> Bool
  ;; determines if the basis dimensions are the same
  ;; assumption: the dimensions are in canonical order
  (define (same-basis? left right)
    (let ([l-dims (blade-dims left)]
          [r-dims (blade-dims right)])
      (equal? l-dims r-dims)))
  ;; Blade Blade -> Blade
  ;; combines two blades with the same basis elements
  ;; assumption: the blades have the same basis dimensions
  (define (simplify left right)
    (let ([l-coef (blade-coef left)]
          [r-coef (blade-coef right)])
      (make-blade (+ l-coef r-coef) (blade-dims left))))
  (let outer-loop ([simplified '()] [blades blades])
    (if (empty? blades)
        (if (empty? simplified)
            (list zero-blade)
            simplified)
        (let inner-loop ([current (first blades)] [passed-blades '()] [rem-blades (rest blades)])
          (if (empty? rem-blades)
              (outer-loop (cons current simplified)
                          passed-blades)
              (let ([next-blade (first rem-blades)])
                (if (same-basis? current next-blade)
                    (inner-loop (simplify current next-blade)
                                passed-blades
                                (rest rem-blades))
                    (inner-loop current
                                (cons next-blade passed-blades)
                                (rest rem-blades)))))))))
