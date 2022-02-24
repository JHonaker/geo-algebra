#lang racket

(provide make-diagonal-metric
         make-conformal-metric
         lookup-metric
         metric-factory)

(define multivector-maker (make-parameter #f))
(define blade-maker (make-parameter #f))

;; A ProductMetric is an interface that implements
;;
;; - lookup-metric : ProductMetric (List-of Dim) (List-of Dim) -> BasisBlade
;; - store-metric-value! : ProductMetric (List-of Dim) (List-of Dim) -> BasisBlade
;;
;; The default should be 0.0

;; - make-metric : Number -> ProductMetric

;;;; Utilities

;; dims->index : List-of Dim -> Integer
;; converts a list of dims to an integer used for storing and retrieving the metric value
(define (dims->index dims)
  (if (empty? dims)
      0
      ;; Dimensions are mapped to 2^x
      (apply + (map (λ (x) (expt 2 x)) dims))))

;;;; Interfaces


(define metric-interface
  (interface ()
    ;; lookup : (List-of Dim) (List-of Dim) -> BasisBlade
    lookup
    ;; store-value! : (List-of Dim) (List-of Dim) Blade -> Void
    store-value!))

;;;; Metrics

;; A NullMetric-over X is a Class that represents the simplest metric over an algebraic structure X.
;;  (new null-metric% [zero-el X])
;; The metric returns a zero-like element for any and all pairs of elements.
(define null-metric%
  (class* object% (metric-interface)
    (init zero-el)
    (super-new)


    ;; zero-element : X
    ;; The zero-like element that the base algebra is over
    ;; For real numbers this would be 0,
    ;; for complex, 0+0i,
    ;; etc.
    ;; These examples would all be covered by 0 though, by Scheme's numeric tower.
    ;; However, if we want more generic algebras, we need this.
    (define zero-element zero-el)
    
    ;; The main hash table that store the metric results
    ;; Extended by children to be non-null
    (define metric-hash (make-hash))

    ;; lookup-metric : (List-of Dim) (List-of Dim) -> Number
    ;; looks up the value of a product on the metric table
    (define/public (lookup-metric metric l-dims r-dims)
      (let ([l-index (dims->index l-dims)]
            [r-index (dims->index r-dims)])
        (hash-ref metric-hash (cons l-index r-index) zero-element)))

    ;; store-metric-value! : (List-of Dim) (List-of Dim) Element -> Element
    ;; stores a value in the metric table
    (define/private (store-metric-value! l-dims r-dims val)
      (let ([l-index (dims->index l-dims)]
            [r-index (dims->index r-dims)])
        (hash-set! metric-hash (cons l-index r-index) val)))

    
    ))


;; make-empty-metric : -> ProductMetric
;; creates a new product metric initialized to always return 0.0
(define (make-empty-metric) (make-hash))

;; lookup-metric : ProductMetric (List-of Dim) (List-of Dim) -> Number
;; looks up the value of a product on the metric table
(define (lookup-metric metric l-dims r-dims)
  (let ([l-index (dims->index l-dims)]
        [r-index (dims->index r-dims)])
    (hash-ref metric (cons l-index r-index) 0.0)))

;; store-metric-value! : ProductMetric (List-of Dim) (List-of Dim) Number -> Number
;; stores a value in the metric table
(define (store-metric-value! metric l-dims r-dims val)
  (let ([l-index (dims->index l-dims)]
        [r-index (dims->index r-dims)])
    (hash-set! metric (cons l-index r-index) val)))

;;;; Utilities

;; List-of X -> (values (List-of X) OddEven)
;; returns two values:
;;   - A sorted list of X
;;   - A symbol 'odd or 'even denoting that an odd or even number
;;     of swaps were necessary to sort the list
;; assumptions: this will only be called to sort an even number of elements
;; this is only expected be called by a metric builder
(define (gnome-sort xs)
  (define (flip-parity x)
    (case x
      [(odd) 'even]
      [(even) 'odd]))
  (let loop ([head empty] [tail xs] [parity 'even])
    (cond [(empty? tail)
           (values (reverse head) parity)]
          [(empty? head)
           (loop (list (first tail)) (rest tail) parity)]
          [else
           (let ([a (first head)]
                 [b (first tail)])
             ;; The elements are in order so just move b over
             (cond [(< a b) (loop (cons b head)
                                  (rest tail)
                                  parity)]
                   ;; The elements are the same, so just move b over
                   [(= a b) (loop (cons b head)
                                  (rest tail)
                                  parity)]
                   ;; The elements are in the opposite order
                   ;; Swap them, then back up one step on head
                   [(> a b) (loop (rest head)
                                  (list* b a (rest tail))
                                  (flip-parity parity))]))])))

;; apply-metric : Dims Dims AList(DimPair . Number) -> (v-of Dims Number)
;; applies the metric in an alist to an unsimplified list of dimensions
;; assumptions: this will only be called to sort an even number of elements
;; this is only expected be called by a metric builder
(define (apply-metric l-dims r-dims metric-alist)
  (define-values (sorted-dims parity) (gnome-sort (append l-dims r-dims)))
  (define (simplify-dims dims dim-acc c)
    (if (= c 0)
        (values '() 0.0)
        (cond [(empty? dims) (values (reverse dim-acc) c)]
              [(empty? (rest dims)) (values (reverse (cons (first dims) dim-acc)) c)]
              [else
               (let* ([a (first dims)]
                      [b (second dims)]
                      [metric-val (assoc (cons a b) metric-alist)])
                 (if metric-val
                     (simplify-dims (rest (rest dims))
                           dim-acc
                           (* c (cdr metric-val)))
                     (simplify-dims (rest dims)
                           (cons a dim-acc)
                           c)))])))
  (let-values ([(dims coef) (simplify-dims sorted-dims empty 1.0)])
    (if (symbol=? parity 'even)
        ((multivector-maker) (list ((blade-maker) coef dims)))
        ((multivector-maker) (list ((blade-maker) (- coef) dims))))))

;; cache-metric! : ProductMetric Integer AList-> Void
;; effect: populates the metric with the basis blade products
(define (cache-metric! metric dimension metric-alist)
  (for* ([left (in-combinations (range 0 dimension))]
         [right (in-combinations (range 0 dimension))])
    (let ([b (apply-metric left right metric-alist)])
      (store-metric-value! metric left right b))))

;; Integer Integer Number -> AList
;; creates an a-list with ((index . index) . v) n times
(define (diagonal-helper n start v)
  (build-list n (λ (i)
                  (let ([idx (+ i start)])
                    (cons (cons idx idx) v)))))

;; make-diagonal-metric : Integer Integer Integer -> ProductMetric
;; creates a diagonal metric with signature (P, N, Z)
;; where the first P dimensions square to 1
;;       the next N dimensions square to -1
;;       the next Z dimensions square to 0
(define (make-diagonal-metric P N Z)
  (make-general-metric (+ P N Z) (append (diagonal-helper P 0 1.0)
                                         (diagonal-helper N P -1.0)
                                         (diagonal-helper Z (+ P N) 0.0))))

;; make-general-metric : Integer AList -> ProductMetric
;; creates a general metric specified by an a-list
(define (make-general-metric dimension metric-alist)
  (let ([metric (make-empty-metric)])
    (cache-metric! metric dimension metric-alist)
    metric))

;; make-conformal-metric : Integer -> ProductMetric
;; creates the geometric product metric for an n-dimensional conformal space
;;      0  1  2  o  inf
;;   0  1  0  0  0  0
;;   1  0  1  0  0  0
;;   2  0  0  1  0  0
;;   o  0  0  0  0 -1
;; inf  0  0  0 -1  0
;; 
(define (make-conformal-metric dimension)
  (let* ([o-dim (list dimension)]
         [inf-dim (list (+ dimension 1))]
         [metric (make-diagonal-metric dimension 0 2)])
    (store-metric-value! metric o-dim inf-dim (multivector
                                               (list (make-blade -1.0 '())
                                                     (make-blade 1.0 (append o-dim inf-dim)))))
    (store-metric-value! metric inf-dim o-dim (multivector
                                               (list (make-blade -1.0 '())
                                                     (make-blade -1.0 (append o-dim inf-dim)))))
    metric))

(define ((metric-factory metric-maker . args) mv-maker b-maker zero-mv)
  (parameterize ([multivector-maker mv-maker]
                 [blade-maker b-maker])
    (apply metric-maker args)))
