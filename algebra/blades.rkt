#lang racket

(provide (all-defined-out))

(require "metric-defs.rkt")
(require "../generic-arith.rkt")

(require memo)

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
  #:transparent
  #:methods gen:custom-write
  [(define write-proc blade-print)])
;; A BasisBlade is a structure
;;  (struct Number List-of Dim Integer)
;; interpretation: a basis blade is one of the fundamental building blocks of
;; geometric algebra. It represents a weighted, oriented graded subspace of the
;; generating field.

;; Number (List-of Dim) -> Blade
;; creates a blade
(define (make-blade coef dims)
  (blade coef dims (length dims)))

;; zero-blade : Metric -> Blade
;; creates the zero-blade associated with a metric
(define/memoize (zero-blade metric)
  (make-blade (send metric zero-element) '()))

;; one-blade : Metric -> Blade
;; creates the one-blade associated with a metric
(define/memoize (one-blade metric)
  (make-blade (send metric one-element) '()))

;; neg-one-blade : Metric -> Blade
;; creates the neg-one-blade associated with a metric
(define/memoize (neg-one-blade metric)
  (neg (one-blade metric)))

;; Number Dim* -> Blade
;; a shorthand for creating blades
(define (b: c . dims)
  (make-blade c dims))

;; filter-grade : List-of Blade Integer -> List-of Blade
;; filters a list of blades to only those of the specified grade
(define (filter-grade list-of-blades grade)
  (filter (λ (b) (= grade (blade-grade b)))
          list-of-blades))

;; List-of Blade -> List-of Blade
;; sorts a list of blades by length and then by numeric value
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
;; simplifies a list of blades by combining basis elements
(define (simplify-blades blades #:zero zero)
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
      (make-blade (add l-coef r-coef) (blade-dims left))))
  ;; List-of Blade -> List-of Blade
  ;; filters out the blades that have a zero coefficient
  (define (filter-zero-coef-blades lob)
    (filter (λ (b) (not (eq-zero? (blade-coef b))))
            lob))
  (let outer-loop ([simplified '()] [blades blades])
    (if (empty? blades)
        (if (empty? simplified)
            (list zero)
            (let ([non-zero-blades (filter-zero-coef-blades simplified)])
              (if (empty? non-zero-blades)
                  (list zero)
                  non-zero-blades)))
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

;;;; Blade Operations

;; Blade+ -> List-of Blade
;; returns the unsimplified list of blades from a sum
(define (b:+ . xs)
  (if (empty? xs)
      (error 'b:+ "No blades passed to +")
      xs))

;; Blade+ -> List-of Blade
;; returns the unsimplified list of blades from a subtraction
;; (first xs) - (sum (rest xs))
(define (b:- . xs)
  (if (empty? xs)
      (error 'b:- "No blades passed to -")
      (cons (first xs) (map neg (rest xs)))))

;; Blade -> Blade
;; returns a blade with the negated coefficient
(define (b:neg b)
  (make-blade (neg (blade-coef b))
              (blade-dims b)))

;; Blade Number -> Blade
;; scales a blade's coefficient by c
(define (b:scale b c)
  (make-blade (mul (blade-coef b) c)
              (blade-dims b)))

;; Metric Blade -> Blade
;; computes the versor inverse of a blade
(define (b:inv metric b)
  (let ([sqnorm (blade-coef (b:metric-product metric b (b:reverse b)))])
    (b:scale b (div 1 sqnorm))))

;; Metric Blade Blade -> Blade
;; multiply x by the inverse of y
(define (b:/ metric x y)
  (b:metric-product metric x (b:inv metric y)))

;;; Products

;; Blade Blade -> Blade
;; computes a metric product of two basis blades
;; Geometric product is (b:metric-product geo-metric left right)
;; Outer product is (b:metric-product null-metric left right)
(define (b:metric-product metric left right)
  (let* ([l-coef (blade-coef left)]
         [l-dims (blade-dims left)]
         [r-coef (blade-coef right)]
         [r-dims (blade-dims right)]
         [result-blade (metric-lookup metric l-dims r-dims)])
    (b:scale result-blade (mul l-coef r-coef))))

;; Blade Blade -> Blade
;; computes the left contraction of a and b
(define (b:contractl metric a b)
  (let ([a-grade (blade-grade a)]
        [b-grade (blade-grade b)])
    (if (<= a-grade b-grade)
        (let ([gp (b:metric-product metric a b)])
          (if (= (blade-grade gp) (- b-grade a-grade))
              gp
              (zero-blade metric)))
        (zero-blade metric))))

;; Blade Blade -> Blade
;; computes the right contraction of a and b
(define (b:contractr metric a b)
  (let ([a-grade (blade-grade a)]
        [b-grade (blade-grade b)])
    (if (>= a-grade b-grade)
        (let ([gp (b:metric-product metric a b)])
          (if (= (blade-grade gp) (- a-grade b-grade))
              gp
              (zero-blade metric)))
        (zero-blade metric))))

;; Blade Blade -> Blade
;; computes the dot product of two blades
(define (b:dot metric a b)
  (let ([a-grade (blade-grade a)]
        [b-grade (blade-grade b)])
    (if (<= a-grade b-grade)
        (b:contractl metric a b)
        (b:contractr metric a b))))

;; Blade Blade -> Blade
;; computes the Hestenes metric product of a and b
(define (b:hest metric a b)
  (let ([a-grade (blade-grade a)]
        [b-grade (blade-grade b)])
    (if (or (= a-grade 0) (= b-grade 0))
        (zero-blade metric)
        (b:dot metric a b))))

;; Blade Blade -> Blade
;; computes the scalar product of a and b
(define (b:scalar metric a b)
  (let ([gp (b:metric-product metric a b)])
    (if (= (blade-grade gp) 0)
        gp
        (zero-blade metric))))

;; ;; Blade Blade -> Multivector
;; ;; computes the commutator product of a and b
;; (define (b:commutator a b)
;;   (mv:scale (mv:- (b:geo a b)
;;                   (b:geo b a))
;;             0.5))

;; ;; Blade Blade -> Multivector
;; ;; computes the anti-commutator product of a and b
;; (define (b:anti-commutator a b)
;;   (mv:scale (mv:+ (b:geo a b)
;;                   (b:geo b a))
;;             0.5))

;;; Grade-dependent sign calculations

;; Integer -> Number
;; returns the multiplier for the reversion of a grade g blade
(define (reversion-multiplier g)
  (expt -1.0 (* g (- g 1) 0.5)))

;; Blade -> Blade
;; computes the reversion of a blade
(define (b:reverse b)
  (b:scale b (reversion-multiplier (blade-grade b))))

;; Integer -> Number
;; returns the multiplier for the involution of a grade g blade
(define (involution-multiplier g)
  (expt -1.0 g))

;; Blade -> Blade
;; returns the involution of a blade
(define (b:involute b)
  (b:scale b (involution-multiplier (blade-grade b))))

;; Integer -> Number
;; returns the multiplier for the Clifford conjugation of a grade g blade
(define (conjugation-multiplier g)
  (expt -1.0 (* g (+ g 1) 0.5)))

;; Blade -> Blade
;; returns the Clifford conjugate of a blade
(define (b:conjugate b)
  (b:scale b (conjugation-multiplier (blade-grade b))))
