#lang racket

; defines the interface for a generic algebraic structure

(provide algebra<%>

         g:=
                  
         element->base
         promote-to
         same-algebra?
         algebra-of
         el-ref

         element-of?
         ->element

         define-unary-ops
         define-unary-op-implementation
         define-binary-ops
         define-binary-op-implementation

         install-algebra-operations!)

(require "../generic.rkt"
         (only-in "../utils.rkt" approx-zero?))

(require (for-syntax syntax/parse))

;; An Algebra with domain is D ⊆ X is a collection of operations and axioms/identities.
;;
;; In Racket terms, an Algebra is an instance of a class that implements the algebra<%> interface.
;; It represents the capabilities of elements of the algebraic structure.
;;
;; The generic Algebra structure makes no assumptions about
;; operations. That is left up to more specific algebraic types. The
;; generic algebra interface ensures functionality for:
;;  - Raising an element of a "lower" algebra to this one
;;  - "Lowering" an element of this algebra to the base one
;;  - Referencing information about a particular algebraic element
;;  - A method for testing equality of two elements of the algebra
;;
;; Internally, it is expected that an algebra represents its elements
;; with a struct defined in the class body. This enables us to have
;; algebras whose operations are identical in functionality, but whose
;; elements are distinct.

(define algebra<%>
  (interface ()
    ;; X -> D
    ;; converts a value of the co-domain to an element in the domain
    ;; for R this would be identity
    ;; but for other, richer structures, this might be something else
    ;; like 0 -> 0+0i
    a:base->element

    ;; R -> D
    ;; converts from the real numbers to the D
    a:->element

    ;; D -> X
    ;; converts an element into the base algebra element if possible
    a:element->base

    ;; Any -> Bool
    ;; determines if a value is an element of the algebra
    a:element-of?

    ;; Any -> Algebra
    ;; returns the algebra of an element
    a:algebra-of

    ;; -- Algebra specific signature --
    ;; reference a part of the element
    a:el-ref

    ;; -- Algebra specific signature --
    ;; create an element of the algebra
    a:make-element

    ;; D D -> Bool
    ;; determines if the algebraic elements are the same
    a:=

    ))

;;; Generic Methods

(define/generic (g:= x y))
(define/implementation (g:= [x number?] [y number?])
  (cond [(= 0 x) (approx-zero? y)]
        [(= 0 y) (approx-zero? x)]
        [else (= x y)]))

(define/generic (element->base x))
(define/generic (promote-to x y))
(define/generic (same-algebra? x y))
(define/generic (algebra-of x))
(define/generic (el-ref x))

;;; Operations on Algebras

(define a:base->element-generic (generic algebra<%> a:base->element))
(define a:->element-generic (generic algebra<%> a:->element))
(define a:element->base-generic (generic algebra<%> a:element->base))
(define a:element-of?-generic (generic algebra<%> a:element-of?))
(define a:algebra-of-generic (generic algebra<%> a:algebra-of))
(define a:el-ref-generic (generic algebra<%> a:el-ref))
(define a:=-generic (generic algebra<%> a:=))

(define ((element-of? alg) x)
  (send-generic alg a:element-of?-generic x))

(define (->element alg x)
  (send-generic alg a:->element-generic x))

;;; Syntax for defining implementations of generic methods for algebraic operations

(define-syntax (define-binary-op-implementation stx)
  (syntax-parse stx
    [(_ p? (method-name x y) body ...)
     #'(begin
         (define/implementation (method-name [x p?] [y p?]) body ...)
         (define/implementation (method-name [x number?] [y p?])
           ((lambda (x y) body ...)
            (promote-to y x) y))
         (define/implementation (method-name [x p?] [y number?])
           ((lambda (x y) body ...)
            x (promote-to x y))))]))

(define-syntax (define-unary-op-implementation stx)
  (syntax-parse stx
    [(_ p? (method-name x) body ...)
     #'(begin
         (define/implementation (method-name [x p?]) body ...))]))

(define-syntax (define-binary-ops stx)
  (syntax-parse stx
    [(_ F (gen-op-name alg-op-name) ...)
     #'(let ([f-el? (element-of? F)])
         (define-binary-op-implementation f-el? (gen-op-name x y)
           (send-generic F alg-op-name x y)) ...)]))

(define-syntax (define-unary-ops stx)
  (syntax-parse stx
    [(_ F (gen-op-name alg-op-name) ...)
     #'(let ([f-el? (element-of? F)])
         (define-unary-op-implementation f-el? (gen-op-name x)
           (send-generic F alg-op-name x)) ...)]))

;; Algebra -> Void
;; effect: installs implementations of the algebraic operations enabled by the algebra<%> interface
;; into the generic procedure mechanism

(define (install-algebra-operations! alg)

  (define-unary-ops alg
    (element->base a:element->base-generic))

  (define-binary-ops alg
    (g:= a:=-generic))
  
  (let ([alg-element? (element-of? alg)])
    (define/implementation (same-algebra? [x alg-element?] [c any-type?])
      (alg-element? c))

    (define/implementation (algebra-of [x alg-element?])
      alg)

    (define/implementation (promote-to [x alg-element?] [y any-type?])
      (cond
        [(number? y)
         (send-generic alg a:->element-generic y)]
        [(alg-element? y) y]
        [((element-of? (get-field base-field x)) y)
         (send-generic alg a:base->element-generic y)]
        [else (error "Could not promote second argument to the first")]))

    (define/implementation (el-ref [x alg-element?])
      (lambda args (send-generic alg a:el-ref-generic x . args)))))
