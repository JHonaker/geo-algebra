#lang racket

; defines the structure for a geometric algebra over a field with an orthongonal metric tensor

(require "../generic.rkt"
         "../generic-arith.rkt"
         "../utils.rkt"
         "metric-defs.rkt"
         "blades.rkt"
         "algebra.rkt"
         "field.rkt"
         "real-numbers.rkt"
         (only-in "algebra.rkt"
                  g:=
                  element-of?))

(require (for-syntax syntax/parse))

(provide geometric-algebra%

         geo
         wedge
         contractl
         contractr
         dot
         hest
         scalar

         rev
         involute
         conjugate

         geo*
         wedge*

         install-just-geo-alg-operations!
         install-geo-alg-operations!

         define-geo-algebra)

(define geometric-algebra%
  (class* object% (field<%>)
    (init-field [p 0] [q 0] [r 0] [base-field (new real-numbers%)])
    (super-new)

    (define base-one (send base-field f:one-element))
    (define base-zero (send base-field f:zero-element))
    (define base-neg-one (neg base-one))
    

    (define zero-blade (make-blade base-zero '()))
    (define one-blade (make-blade base-one '()))
    (define neg-one-blade (make-blade base-neg-one '()))

    ;;;; Metric definitions

    (field
     ;; NullMetric
     ;; defines the "metric" over the non-metric wedge product
     [wedge-metric (new diagonal-metric%
                        [p 0] [q 0] [r (+ p q r)]
                        [pos-el base-one]
                        [neg-el base-neg-one]
                        [zero-el base-zero]
                        [element-fn make-blade])]
     
     ;; DiagonalMetric
     ;; defines the metric for the geometric product
     ;; the other metric products are derived from the geometric product
     [metric (new diagonal-metric%
                  [p p] [q q] [r r]
                  [pos-el base-one]
                  [neg-el base-neg-one]
                  [zero-el base-zero]
                  [element-fn make-blade])])

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
      ;; A Multivector is a structure
      ;;  (struct List-of BasisBlade)
      ;; where blades are ordered by grade and present dimensions
      ;; interpretation: A multivector represents the general structure of a mixed
      ;; grade object in geometric algebra.
      #:transparent
      #:methods gen:custom-write
      [(define write-proc multivector-print)])

    ;; List-of Blade -> Multivector
    ;; creates a multivector with blades in the proper order
    (define (make-multivector blades)
      (if (empty? blades)
          zero-mv
          (multivector (sort-blades (simplify-blades blades #:zero zero-blade)))))

    ;; Blade -> Multivector
    ;; converts a blade to a multivector
    (define (blade->multivector b)
      (multivector (list b)))


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

    ;; Multivector -> F
    ;; converts a scalar multivector to the base field value
    (define (multivector->base mv)
      (if (mv:is-scalar? mv)
          (blade-coef (first (multivector-blades mv)))
          (error 'multivector->base "~a is not a scalar multivector" mv)))
    
    ;; Multivector -> Multivector
    ;; filters the grades of a multivector
    (define/public (select-grade mv g)
      (make-multivector (filter (λ (b) (= g (blade-grade b)))
                                (multivector-blades mv))))

    ;;;; Multivector Operations

    ;; Multivector+ -> Multivector
    ;; returns the sum of left and right
    (define/public (mv:+ . xs)
      (if (empty? xs)
          (error 'mv:+ "No multivectors passed to +")
          (make-multivector (flatten (map multivector-blades xs)))))

    ;; Multivector+ -> Multivector
    ;; returns (first xs) - (sum (rest xs))
    (define/public (mv:- . xs)
      (send/apply this mv:+ (first xs) (map/method this mv:neg (rest xs))))

    ;; Multivector -> Multivector
    ;; returns the negation of each of the multivectors blades
    (define/public (mv:neg x)
      (multivector (map b:neg (multivector-blades x))))
    
    ;; Multivector Number -> Multivector
    ;; scales a multivectors coefficients by c
    (define/public (mv:scale mv c)
      (multivector (map (curryr b:scale c) (multivector-blades mv))))

    ;; Multivector -> Multivector
    ;; computes the versor inverse of a multivector
    (define/public (mv:inv mv)
      (let ([sqnorm (multivector->base (mv:geo mv (mv:reverse mv)))])
        (mv:scale mv (div 1 sqnorm))))

    ;; Multivector Multivector -> Multivector
    ;; multiply x by the inverse of y
    (define/public (mv:/ x y)
      (mv:geo x (mv:inv y)))

    ;;; Grade Dependent Computations

    ;; Multivector -> Multivector
    ;; comptues the reverse of the multivector mv
    (define/public (mv:reverse mv)
      (make-multivector (map b:reverse (multivector-blades mv))))

    ;; Multivector -> Multivector
    ;; comptues the involution of the multivector mv
    (define/public (mv:involute mv)
      (make-multivector (map b:involute (multivector-blades mv))))

    ;; Multivector -> Multivector
    ;; comptues the conjugate of the multivector mv
    (define/public (mv:conjugate mv)
      (make-multivector (map b:conjugate (multivector-blades mv))))

    ;; Multivector -> Multivector
    ;; computes the multivector squared norm
    (define/public (mv:sqnorm A)
      (mv:geo A (mv:reverse A)))

    ;; Multivector -> Number
    ;; computes the norm of a multivector
    (define/public (mv:norm A)
      (sqrt (multivector->base (mv:sqnorm A))))

    ;;; Products

    ;; [Blade Blade -> Multivector] -> [Multivector Multivector -> Multivector]
    ;; lifts a blade product onto multivectors via bilinearity
    (define ((lift-product->multivector op) left right)
      (let ([unsimplified
             (for*/list ([l (in-list (multivector-blades left))]
                         [r (in-list (multivector-blades right))])
               (op l r))])
        (make-multivector unsimplified)))

    (define/public (mv:wedge left right)
      ((lift-product->multivector (curry b:metric-product wedge-metric)) left right))
    (define/public (mv:geo left right)
      ((lift-product->multivector (curry b:metric-product metric)) left right))
    (define/public (mv:contractl left right)
      ((lift-product->multivector (curry b:contractl metric)) left right))
    (define/public (mv:contractr left right)
      ((lift-product->multivector (curry b:contractr metric)) left right))
    (define/public (mv:dot left right)
      ((lift-product->multivector (curry b:dot metric)) left right))
    (define/public (mv:hest left right)
      ((lift-product->multivector (curry b:hest metric)) left right))
    (define/public (mv:scalar left right)
      ((lift-product->multivector (curry b:scalar metric)) left right))

    (define pseudoscalar (mv: (b: 1.0 0 1 2 3 4)))
    (define inv-pseudoscalar (mv: (b: -1.0 0 1 2 3 4)))
    
    ;; Multivector Multivector -> Multivector
    ;; compute the dual of a multivector with respect to a pseudoscalar
    (define/public (mv:dual mv)
      (mv:contractl mv inv-pseudoscalar))

    ;; Multivector Multivector -> Multivector
    ;; unduals a multivector with respect to a pseudoscalar
    (define/public (mv:undual mv)
      (mv:contractl mv pseudoscalar))

    ;;;; The element creation interface

    ;; make-element : List-of (list Coefficient . BladeDims) -> Multivector
    ;; takes a list of coefficients and blade dimensions  and returns the multivector of the algebra
    (define/public (a:make-element . blades)
      (make-multivector (map (curry apply b:) blades)))


    ;; Implementation of field<%>

    (define/public (f:zero-element) zero-mv)
    (define/public (f:one-element) one-mv)
    
    ;; F -> Multivector
    ;; converts an element of the base field to a multivector
    (define/public (a:base->element c)
      (multivector (list (make-blade c '()))))

    (define/public (a:element->base e)
      (multivector->base e))

    ;; X -> Multivector
    ;; converts a number to a multivector through the proper chain
    ;; of nested algebras where X is the type of the lowest algebra
    (define/public (a:->element c)
      (let ([base-el (send base-field a:->element c)])
        (a:base->element base-el)))

    (define/public (a:element-of? val)
      (multivector? val))

    (define/public (a:algebra-of _)
      this)

    (define/public (f:+ left right)
      (mv:+ left right))

    (define/public (f:- left right)
      (mv:- left right))

    (define/public (f:* left right)
      (mv:geo left right))

    (define/public (f:/ left right)
      (mv:/ left right))

    (define/public (f:+-inverse x)
      (mv:neg x))

    (define/public (f:*-inverse x)
      (mv:inv x))

    (define/public (f:scale el x)
      (mv:scale el x))

    (define/public (a:el-ref x #:as-multivector? [as-multivector? #f] . list-of-dims)
      (for/fold ([blades '()]
                 #:result (if as-multivector?
                              (multivector (reverse blades))
                              (reverse blades)))
                ([dims (in-list list-of-dims)])
        (let ([the-blade (findf (λ (b) (equal? dims (blade-dims b))) (multivector-blades x))])
          (if the-blade
              (if as-multivector?
                  (cons the-blade blades)
                  (cons (blade-coef the-blade) blades))
              (if as-multivector?
                  (cons zero-blade blades)
                  (cons 0 blades))))))

    ;; HACK This is only here to get the testing of dual numbers == 0 to work
    ;; TODO generalize this by making a special dual-number algebra class
    ;; MV Number -> Bool
    ;; Tests if the scalar part is equal to a number
    (define/public (a:= x y)
      (let loop ([xs (multivector-blades x)] [ys (multivector-blades y)])
        (cond [(and (empty? xs) (empty? ys)) #t]
              [(or (empty? xs) (empty? ys)) #f]
              [else
               (match-let ([(blade x-coef x-dims _) (first xs)]
                           [(blade y-coef y-dims _) (first ys)])
                 (if (and (g:= x-coef y-coef)
                          (equal? x-dims y-dims))
                     (loop (rest xs) (rest ys))
                     #f))])))

    ))

(define/generic (geo x y))
(define/generic (wedge x y))
(define/generic (contractl x y))
(define/generic (contractr x y))
(define/generic (dot x y))
(define/generic (hest x y))
(define/generic (scalar x y))

(define/generic (rev x))
(define/generic (involute x))
(define/generic (conjugate x))

(define (geo* . xs) (foldl geo (one-like (first xs)) xs))
(define (wedge* . xs) (foldl wedge (one-like (first xs)) xs))

(define (install-just-geo-alg-operations! GA)
  (let ([multivector? (element-of? GA)])
    ;;;; Implementing the generic method interface

    (define-binary-op-implementation multivector? (geo x y) (send GA mv:geo x y))
    (define-binary-op-implementation multivector? (wedge x y) (send GA mv:wedge x y))
    (define-binary-op-implementation multivector? (contractl x y) (send GA mv:contractl x y))
    (define-binary-op-implementation multivector? (contractr x y) (send GA mv:contractr x y))
    (define-binary-op-implementation multivector? (dot x y) (send GA mv:dot x y))
    (define-binary-op-implementation multivector? (hest x y) (send GA mv:hest x y))
    (define-binary-op-implementation multivector? (scalar x y) (send GA mv:scalar x y))
    ;; (define-binary-op-implementation (commutator x y) (send GA mv:commutator x y))
    ;; (define-binary-op-implementation (anticommutator x y) (send GA mv:anticommutator x y))
    
    (define/implementation (rev [x multivector?]) (send GA mv:reverse x))
    (define/implementation (involute [x multivector?]) (send GA mv:involute x))
    (define/implementation (conjugate [x multivector?]) (send GA mv:conjugate x))

    (define/implementation (sqnorm [x multivector?]) (send GA mv:sqnorm x))
    (define/implementation (norm [x multivector?]) (send GA mv:norm x))
    
    #;(define/implementation (->number [x multivector?]) (multivector->base x))


    ))

(define (install-geo-alg-operations! GA)
  (install-field-operations! GA)
  (install-just-geo-alg-operations! GA))

(define-syntax (define-geo-algebra stx)
  (syntax-parse stx 
    [(_ name prefix args ...)
     #'(begin
         (define name (new geometric-algebra% args ...))
         (install-geo-alg-operations! name)
         (... (define-syntax (prefix -stx)
                (syntax-parse -stx
                  [(_ (coef dims ...) ...)
                   #'(send name a:make-element (list coef dims ...) ...
                           )])
                )))]))


