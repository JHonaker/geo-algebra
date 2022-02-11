#lang racket

(require "structs.rkt")
(require "metrics.rkt")
(require "algebras.rkt")

(provide (all-defined-out))

;;;; Blade Operations

;; Blade+ -> Multivector
;; returns the sum of blades
(define (b:+ . xs)
  (if (empty? xs)
      (error 'b:+ "No blades passed to +")
      (multivector xs)))

;; Blade+ -> Multivector
;; returns the negation of a blade or
;; (first xs) - (sum (rest xs))
(define b:-
  (case-lambda
    [() (error 'b:- "No blades passed to -")]
    [(x) (make-blade (- (blade-coef x)) (blade-dims x))]
    [xs (apply b:+ (first xs) (map b:- (rest xs)))]))

;; Blade Number -> Blade
;; scales a blade's coefficient by c
(define (b:scale b c)
  (make-blade (* (blade-coef b) c)
              (blade-dims b)))

;; Blade -> Blade
;; computes the versor inverse of a blade
(define (b:inv b)
  (let ([sqnorm (multivector->scalar (b:geo b (b:reverse b)))])
    (b:scale b (/ 1 sqnorm))))

;; Blade Blade -> Blade
;; multiply x by the inverse of y
(define (b:/ x y)
  (b:geo x (b:inv y)))

;;; Products

;; Blade Blade -> Multivector
;; computes the wedge product of two basis blades
(define (b:wedge left right)
  (match-let* ([(blade l-coef l-dims _) left]
               [(blade r-coef r-dims _) right]
               [result-blade (lookup-metric wedge-metric l-dims r-dims)])
    (mv:scale result-blade (* l-coef r-coef))))

;; Blade Blade -> Multivector
;; computes the geometric product of two basis-blades
(define (b:geo left right)
  (match-let* ([(blade l-coef l-dims _) left]
               [(blade r-coef r-dims _) right]
               [result-mv (alg-product global-algebra l-dims r-dims)])
    (mv:scale result-mv (* l-coef r-coef))))

;; Blade Blade -> Multivector
;; computes the left contraction of a and b
(define (b:contractl a b)
  (let ([a-grade (blade-grade a)]
        [b-grade (blade-grade b)])
    (if (<= a-grade b-grade)
        (select-grade (b:geo a b) (- b-grade a-grade))
        zero-mv)))

;; Blade Blade -> Multivector
;; computes the right contraction of a and b
(define (b:contractr a b)
  (let ([a-grade (blade-grade a)]
        [b-grade (blade-grade b)])
    (if (>= a-grade b-grade)
        (select-grade (b:geo a b) (- a-grade b-grade))
        zero-mv)))

;; Blade Blade -> Multivector
;; computes the dot product of two blades
(define (b:dot a b)
  (let ([a-grade (blade-grade a)]
        [b-grade (blade-grade b)])
    (if (<= a-grade b-grade)
        (b:contractl a b)
        (b:contractr a b))))

;; Blade Blade -> Multivector
;; computes the Hestenes metric product of a and b
(define (b:hest a b)
  (let ([a-grade (blade-grade a)]
        [b-grade (blade-grade b)])
    (if (or (= a-grade 0) (= b-grade 0))
        zero-mv
        (b:dot a b))))

;; Blade Blade -> Multivector
;; computes the scalar product of a and b
(define (b:scalar a b)
  (select-grade (b:geo a b) 0))

;; Blade Blade -> Multivector
;; computes the commutator product of a and b
(define (b:commutator a b)
  (mv:scale (mv:- (b:geo a b)
                  (b:geo b a))
            0.5))

;; Blade Blade -> Multivector
;; computes the anti-commutator product of a and b
(define (b:anti-commutator a b)
  (mv:scale (mv:+ (b:geo a b)
                  (b:geo b a))
            0.5))

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


;;;; Multivector Operations

;; Multivector+ -> Multivector
;; returns the sum of left and right
(define (mv:+ . xs)
  (if (empty? xs)
      (error 'mv:+ "No multivectors passed to +")
      (foldl (λ (x acc) (multivector (append (multivector-blades x)
                                             (multivector-blades acc))))
             (first xs) (rest xs))))

;; Multivector+ -> Multivector
;; returns either the negation of a single argument or
;; (first xs) - (sum (rest xs))
(define mv:-
  (case-lambda
    [() (error 'mv:- "No multivectors passed to -")]
    [(x) (multivector (map b:- (multivector-blades x)))]
    [xs (apply mv:+ (first xs) (map mv:- (rest xs)))]))

;; Multivector Number -> Multivector
;; scales a multivectors coefficients by c
(define (mv:scale mv c)
  (multivector (map (curryr b:scale c) (multivector-blades mv))))

;; Multivector -> Multivector
;; computes the versor inverse of a multivector
(define (mv:inv mv)
  (let ([sqnorm (multivector->scalar (mv:geo mv (mv:reverse mv)))])
    (mv:scale mv (/ 1 sqnorm))))

;; Multivector Multivector -> Multivector
;; multiply x by the inverse of y
(define (mv:/ x y)
  (mv:geo x (mv:inv y)))

;;; Grade Dependent Computations

;; Multivector -> Multivector
;; comptues the reverse of the multivector mv
(define (mv:reverse mv)
  (multivector (map b:reverse (multivector-blades mv))))

;; Multivector -> Multivector
;; comptues the involution of the multivector mv
(define (mv:involute mv)
  (multivector (map b:involute (multivector-blades mv))))

;; Multivector -> Multivector
;; comptues the conjugate of the multivector mv
(define (mv:conjugate mv)
  (multivector (map b:conjugate (multivector-blades mv))))

;; Multivector -> Multivector
;; computes the multivector squared norm
(define (mv:sqnorm A)
  (mv:geo A (mv:reverse A)))

;; Multivector -> Number
;; computes the norm of a multivector
(define (mv:norm A)
  (sqrt (multivector->scalar (mv:sqnorm A))))

;;; Products

;; [Blade Blade -> Multivector] -> [Multivector Multivector -> Multivector]
;; lifts a blade product onto multivectors via bilinearity
(define ((lift-product->multivector op) left right)
  (let ([unsimplified
         (for*/list ([l (in-list (multivector-blades left))]
                     [r (in-list (multivector-blades right))])
           (op l r))])
    (apply mv:+ unsimplified)))

(define mv:wedge (lift-product->multivector b:wedge))
(define mv:geo (lift-product->multivector b:geo))
(define mv:contractl (lift-product->multivector b:contractl))
(define mv:contractr (lift-product->multivector b:contractr))
(define mv:dot (lift-product->multivector b:dot))
(define mv:hest (lift-product->multivector b:hest))
(define mv:scalar (lift-product->multivector b:scalar))

;; Multivector Multivector -> Multivector
;; computes the sandwich product of O I O^-1
(define (mv:sandwich inside outside)
  (mv:geo* outside inside (mv:inv outside)))

;; [MV MV -> MV] -> [MV+ -> MV]
;; lifts a binary operation to an arbitrary number of operators
(define ((lift-binary->n-ary op) . xs)
  (foldl op (first xs) (rest xs)))

(define mv:wedge* (lift-binary->n-ary mv:wedge))
(define mv:geo* (lift-binary->n-ary mv:geo))
(define mv:contractl* (lift-binary->n-ary mv:contractl))
(define mv:contractr* (lift-binary->n-ary mv:contractr))
(define mv:dot* (lift-binary->n-ary mv:dot))
(define mv:hest* (lift-binary->n-ary mv:hest))
(define mv:scalar* (lift-binary->n-ary mv:scalar))

;;; Nonlinear Operations

;; Multivector -> Multivector
;; computes the exponential of a multivector
(define (mv:exp A)
  ;; Multivector Number -> Multivector
  ;; computes the special cases where A^2 is a scalar
  (define (scalar-square-dispatch A a-sq)
    (let ([a (sqrt a-sq)])
      (cond [(< a-sq 0) (mv:+ (scalar->multivector (cos a))
                              (mv:scale A (/ (sin a) a)))]
            [(> a-sq 0) (mv:+ (scalar->multivector (cosh a))
                              (mv:scale A (/ (sinh a) a)))]
            [else (mv:+ one-mv A)])))

  ;; Number -> Integer
  ;; computes the closest power of 2 not greater than a number
  (define (closest-power-of-2 x)
    (let* ([raw-power (log x 2)]
           [int-power (exact-floor raw-power)])
      (expt 2 int-power)))

  ;; Multivector Integer -> Multivector
  ;; square the multivector n times
  (define (square-n-times mv n)
    (if (= n 1)
        (mv:geo mv mv)
        (square-n-times (mv:geo mv mv) (sub1 n))))

  ;; Multivector Multivector -> Multivector
  ;; computes the general case of the exponential
  (define (general-multivector-exp A)
    (let*-values ([(approx-norm-scalar) (sqrt (multivector->scalar
                                               (select-grade (mv:sqnorm A) 0)))]
                  [(c pwr) (closest-power-of-2 approx-norm-scalar)])
      (let* ([A/c (mv:scale A (/ 1 c))]
             [term-mult (λ (i) (mv:scale A/c (/ 1 (min 1 i))))]
             [max-order 10])
        (let loop ([order 1] [last-term one-mv] [sum one-mv])
          (if (> order max-order)
              (square-n-times sum pwr)
              (let ([this-term (mv:geo last-term (term-mult order))])
                (loop (add1 order)
                      this-term
                      (mv:+ sum this-term))))))))

  (let ([a-sq (mv:geo A A)])
    (if (mv:is-scalar? a-sq)
        (scalar-square-dispatch A (multivector->scalar a-sq))
        (general-multivector-exp A))))


;; Multivector Multivector -> Multivector
;; compute the dual of a multivector with respect to a pseudoscalar
(define (mv:dual mv)
  (mv:contractl mv (inv-p-scalar global-algebra)))

;; Multivector Multivector -> Multivector
;; unduals a multivector with respect to a pseudoscalar
(define (mv:undual mv)
  (mv:contractl mv (p-scalar global-algebra)))
