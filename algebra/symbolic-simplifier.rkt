#lang racket

; a symbolic simplifier (for polynomials)
; TODO add trig functions (sin, cos, exp, log, etc.)

(provide simplify-poly)

(require (only-in math binomial))
(require racket/trace)

;; In Racket terms, a Polynomial is one of:
;;  - Number
;;  - Symbol
;;  - InternalPoly

;;  - Number
;;  - Symbol
;;  - (+ Polynomial Polynomial)
;;  - (* Polynomial Polynomial)
;;
;; A polynomial is a function of one or more variables that can be computed using only addition and
;; multiplication. A polynomial has a main term, coefficients, and a degree.
;; 

;;

(struct internal-poly (term coefs) #:transparent)
;; An InternalPolynomial is a structure
;;  (struct Symbol Vector-of Coefficient)
;; An internal polynomial is the representation of a complex polynomial. It has a main term,
;; a coefficient vector (in increasing degree order), and a degree
;; 
;; As an example, the polynomial
;;  5x^3 + bx^2 + cx + 1
;; would be represented by
;;  (internal-poly 'x #(1 'c 'b 5))

(module+ test
  (require rackunit)
  (define poly-1 1)
  (define poly-2 'x)
  (define poly-3 (internal-poly 'x #(0 1 2)))
  (define poly-4 (internal-poly 'x #('a 'b)))
  (define poly-5 (internal-poly 'x #(1 'c 'b 5))))

;;;; Constructors

;; make-poly : Symbol Int
;; creates a polynomial of n-th degree with the main term X
(define (make-poly x d)
  (internal-poly x (make-vector (add1 d) 0)))

;; poly : Symbol . Coefficient -> Polynomial
;; creates a polynomial of X with the given coefficients
(define (poly x . coefs)
  (internal-poly x (apply vector coefs)))

;;;; Data Accessors

;; poly? : Any -> Bool
;; determines if x is a polynomial
(define (poly? x)
  (or (number? x)
      (symbol? x)
      (internal-poly? x)))

;; poly-term : Polynomial -> Maybe Symbol
;; returns the main term symbol of a polynomial
(define (poly-term p)
  (cond [(number? p) #f]
        [(symbol? p) p]
        [else (internal-poly-term p)]))


;; poly-coef : Polynomial Int -> Polynomial
;; returns the coefficient of the ith-degree term of p
(define (poly-coef p i)
  (cond [(number? p) (if (= i 0) p 0)]
        [(symbol? p) (if (= i 1) 1 0)]
        [else (if (<= i (poly-degree p))
                  (vector-ref (internal-poly-coefs p) i)
                  0)]))

;; poly-coefs : Polynomial -> Vector-of Coefficient
;; returns the coefficient vector of the polynomial
(define (poly-coefs p)
  (cond [(number? p) (vector p)]
        [(symbol? p) (vector 0 1)]
        [else (internal-poly-coefs p)]))

;; poly-degree : Polynomial -> Integer
;; returns the highest degree of the polynomial p
(define (poly-degree p)
  (cond [(number? p) 0]
        [(symbol? p) 1]
        [else (sub1 (vector-length (internal-poly-coefs p)))]))

;; set-poly-coef! : Polynomial Int Polynomial -> Void
;; effect: sets the i-th degree coefficient of a polynomial to value
(define (set-poly-coef! p i value)
  (cond [(number? p)
         (error 'set-poly-coef! "Cannot set! the coefficient of a constant ~a." p)]
        [(symbol? p)
         (error 'set-poly-coef! "Cannot set! the coefficient of a constant ~a." p)]
        [else
         (if (<= i (poly-degree p))
             (vector-set! (internal-poly-coefs p) i value)
             (error 'set-poly-coef!
                    "Cannot set the ~a coefficient of a ~a degree polynomial."
                    i (poly-degree p)))]))

;; set-poly-coef : Polynomial Int Polynomial -> Polynomial
;; sets the i-th degree coefficient of a polynomial to a value
(define (set-poly-coef p i value)
  (cond [(number? p)
         (if (= i 0)
             value
             (error 'set-poly-coef! "Cannot set the coefficient of a constant ~a." p))]
        [(symbol? p)
         (let ([the-p (make-poly p (max i 1))])
           (set-poly-coef! the-p i value)
           the-p)]
        [else
         (let ([coefs (make-vector (add1 (max (poly-degree p) i)) 0)])
           (for ([d (in-range (add1 (poly-degree p)))]
                 [coef (in-vector (internal-poly-coefs p))])
             (vector-set! coefs d coef))
           (vector-set! coefs i value)
           (internal-poly (internal-poly-term p) coefs))]))

;;; Operation Transformers

;; An OpTransformer is a structure
(struct op-transformer (sym fn))
;;  (op-trans Symbol Function)
;; An operation transformer contains the information on how to transform a symbolic representation of
;; an operation (symbolically represented by sym) into the canonical form using fn.

;;;; External Interface

(define (simplify-poly sexp)
  (canon->prefix (prefix->canon sexp)))

(define (simplify-poly-repl)
  (let loop ()
    (display "simplifier>")
    (displayln (simplify-poly (read)))
    (loop)))

;;;; Main Functions

;; poly+poly : Polynomial Polynomial -> Polynomial
;; adds two polynomials together
(define (poly+poly p q)
  (normalize-poly
   (cond [(number? p)
          (poly-add-const q p)]
         [(number? q)
          (poly-add-const p q)]
         [(poly-term=? p q)
          (poly-add-same-term p q)]
         [(poly-term<? p q)
          (poly-add-const p q)]
         [else ; (poly-term>? p q)
          (poly-add-const q p)])))

;; poly*poly : Polynomial Polynomial -> Polynomial
;; multiplies two polynomials
(define (poly*poly p q)
  (normalize-poly
   (cond [(number? p)
          (poly-mul-const q p)]
         [(number? q)
          (poly-mul-const p q)]
         [(poly-term=? p q)
          (poly-mul-same-term p q)]
         [(poly-term<? p q)
          (poly-mul-const p q)]
         [else ; (poly-term>? p q)
          (poly-mul-const q p)])))

#|
;; poly-expt : Polynomial Number -> Polynomial
;; raises a polynomial to the n-th power
(define (poly-expt-og p n)
  (cond [(= n 0) (if (and (number? p) (= p 0))
                     (error 'poly-exp "Cannot raise zero to zero-th power")
                     1)]
        [(number? p) (expt p n)]
        [else (poly*poly p (poly-expt p (- n 1)))]))
|#

;; poly-expt : Polynomial Number -> Polynomial
;; raises a polynomial to the n-th power
(define (poly-expt p n)
  (cond [(= n 0) (if (and (number? p) (= p 0))
                     (error 'poly-exp "Cannot raise zero to zero-th power")
                     1)]
        [(number? p) (expt p n)]
        [else (poly-expt-binomial p n)]))

;; poly-expt-binomial : Polynomial Number -> Polynomial
;; uses the binomial coefficient to compute the polynomial coefficients of a (expt polynomial n)
(define (poly-expt-binomial p n)
  (define (add-c*p! result c p)
    (if (or (number? p) (not (poly-term=? p result)))
        (set-poly-coef! result 0 (poly+poly (poly-coef result 0)
                                            (poly*poly c p)))
        (for ([i (in-inclusive-range 0 (poly-degree p))])
          (set-poly-coef! result i
                          (poly+poly (poly-coef result i)
                                     (poly*poly c (poly-coef p i)))))))
  (define p-term (poly-term p))
  (define p-degree (poly-degree p))
  ;; First: split p into a + b where
  (let (;; a = k*x^d, b = rest of p
        [a (make-poly p-term p-degree)]
        [b (normalize-poly (apply poly
                                  p-term
                                  (vector->list (vector-drop-right (poly-coefs p) 1))))]
        ;; Vectors of powers of a and b
        [a^n (make-vector (+ n 1))]
        [b^n (make-vector (+ n 1))]
        ;; Initialize the result
        [result (make-poly p-term (* p-degree n))])
    (set-poly-coef! a p-degree (poly-coef p p-degree))
    ;; Second: Compute powers of a^i and b^i for i up to n
    (vector-set! a^n 0 1)
    (vector-set! b^n 0 1)
    (for ([i (in-inclusive-range 1 n)])
      (vector-set! a^n i (poly*poly a (vector-ref a^n (sub1 i))))
      (vector-set! b^n i (poly*poly b (vector-ref b^n (sub1 i)))))
    ;; Third: add the products to the result
    (for ([i (in-inclusive-range 0 n)])
      (add-c*p! result (binomial n i)
                (poly*poly (vector-ref a^n i)
                           (vector-ref b^n (- n i)))))
    result))

;;;; Auxiliary Functions

;; poly-term=? : Polynomial Polynomial -> Bool
;; determines if two polynomials have the same term
(define (poly-term=? p q)
  (define p-term (poly-term p))
  (define q-term (poly-term q))
  (symbol=? p-term q-term))

;; poly-term<? : Polynomial Polynomial -> Bool
;; determines if two polynomials are in alphabetical order according to their main term
(define (poly-term<? p q)
  (define p-term (poly-term p))
  (define q-term (poly-term q))
  (symbol<? p-term q-term))


;; poly+ : Polynomial+ -> Polynomial
;; unary or binary polynomial addition
(define poly+
  (case-lambda
    [(p) p]
    [(p q) (poly+poly p q)]
    [(p q . rs) (apply poly+ (poly+poly p q) rs)]))

;; poly- : Polynomial+ -> Polynomial
;; unary or binary polynomial subtraction
(define poly-
  (case-lambda
    [(p) (poly*poly -1 p)]
    [(p q) (poly+poly p (poly*poly -1 q))]
    [(p . qs) (apply poly+ p (map (curry poly*poly -1) qs))]))

;; poly-add-const : Polynomial Number -> Polynomial
;; adds a constant k to a polynomial
(define (poly-add-const p k)
  (cond [(and (number? k) (= k 0)) p]
        [(and (number? p) (number? k)) (+ p k)]
        [else (set-poly-coef p 0 (poly+poly (poly-coef p 0) k))]))

;; poly-mul-const : Polynomial Number -> Polynomial
;; multiplies a polynomial by a constant k
(define (poly-mul-const p k)
  (define k-number? (number? k))
  (cond [(and k-number? (= k 0)) 0]
        [(and k-number? (= k 1)) p]
        [(and k-number? (number? p)) (* p k)]
        [else (internal-poly (poly-term p)
                             (vector-map (curry poly*poly k) (poly-coefs p)))]))

;; poly-add-same-term : Polynomial Polynomial -> Polynomial
;; adds two polynomials with the same main term
(define (poly-add-same-term p q)
  (define degree-p (poly-degree p))
  (define degree-q (poly-degree q))
  ;; Ensure degree of q is >= degree of p
  (if (< degree-q degree-p)
      (poly-add-same-term q p)
      (let loop ([q q] [deg 0])
        (if (> deg degree-p)
            q
            (loop (set-poly-coef q deg (poly+poly (poly-coef q deg) (poly-coef p deg)))
                  (add1 deg))))))

;; poly-mul-same-term : Polynomial Polynomial -> Polynomial
;; multiplies two polynomials with the same main term
(define (poly-mul-same-term p q)
  (define total-degree (+ (poly-degree p)
                          (poly-degree q)))
  (define out-poly (make-poly (poly-term q) total-degree))
  (for* ([i (in-inclusive-range 0 (poly-degree p))]
         [j (in-inclusive-range 0 (poly-degree q))]
         #:unless (equal? (poly-coef p i) 0)
         #:unless (equal? (poly-coef q j) 0))
    (define out-coef (poly-coef out-poly (+ i j)))
    (define p-coef (poly-coef p i))
    (define q-coef (poly-coef q j))
    (set-poly-coef! out-poly (+ i j)
                    (poly+poly out-coef
                               (poly*poly p-coef q-coef))))
  out-poly)

;; normalize-poly : Polynomial -> Polynomial
;; drop zero terms from the highest degrees
(define (normalize-poly p)
  (cond [(number? p) p]
        [(symbol? p) (poly p 0 1)]
        [else
         (define highest-nonzero-degree
           (vector-argmax identity
                          (vector-map (λ (i c) (if (equal? c 0) 0 i))
                                      (list->vector (inclusive-range 0 (poly-degree p)))
                                      (internal-poly-coefs p))))
         (cond [(<= highest-nonzero-degree 0)
                (normalize-poly (poly-coef p 0))]
               [(< highest-nonzero-degree (poly-degree p))
                (internal-poly (poly-term p)
                               (vector-take (internal-poly-coefs p)
                                            (add1 highest-nonzero-degree)))]
               [else p])]))

;;;; Conversion

;; prefix->canon : S-Exp -> Polynomial
;; converts an s-expression to canonical polynomial form
(define (prefix->canon sexp)
  (cond [(number? sexp) sexp]
        [(symbol? sexp) (poly sexp 0 1)]
        [else (cond [(list? sexp)
                     (let ([transformer (assoc (first sexp) OP-RULES)])
                       (if transformer
                           (apply (second transformer)
                                  (map prefix->canon (rest sexp)))
                           (error 'prefix->canon "Not a polynomial: ~a" sexp)))]
                    [else (error 'prefix->canon "Not a polynomial: ~a" sexp)])]))

;; canon->prefix : Polynomial -> S-Exp
;; converts a polynomial to an s-expression
(define (canon->prefix p)
  (if (number? p)
      p
      (args->prefix
       '+ 0
       (for/list ([deg (in-inclusive-range 0 (poly-degree p))])
         (args->prefix
          '* 1
          (list (canon->prefix (poly-coef p deg))
                (exponent->prefix (poly-term p) deg)))))))

;; exponent->prefix : Polynomial Int -> S-Exp
;; converts a canonical (expt base exponent) form to s-exp
(define (exponent->prefix base exponent)
  (case exponent
    [(0) 1]
    [(1) base]
    [else `(expt ,base ,exponent)]))

;; args->prefix : 
;; converts arg1 op arg2 op ... to prefix form
(define (args->prefix op identity-element args)

  (define useful-args (remove* (list identity-element) args))
  (cond [(empty? useful-args) identity-element]
        [(and (symbol=? op '*)
              (member 0 args)) 0]
        [(empty? (rest useful-args)) (first useful-args)]
        [else (cons op (append-map
                        (λ (expr) (if (starts-with? expr op symbol=?)
                                      (rest expr)
                                      (list expr)))
                        useful-args))]))

(define (starts-with? lst value [eq-test equal?])
  (and (list? lst)
       (eq-test (first lst) value)))


;; OP-RULES : List-of OpTransformer
;; the global list of operation transformers
(define OP-RULES `((+ ,poly+)
                   (- ,poly-)
                   (* ,poly*poly)
                   (expt ,poly-expt)))

(module+ test
  (define sexp-1 '(+ (expt x 2) (* 3 x) 4))
  (define sexp-1->poly (poly 'x 4 3 1))
  (check-equal? (prefix->canon sexp-1)
                sexp-1->poly)

  (let ([r (prefix->canon '(+ 1 (+ x (+ y z))))])
    (define d 15)
    (time (poly-expt r d))
    #;(canon->prefix (poly-expt r d))
    (void)))
