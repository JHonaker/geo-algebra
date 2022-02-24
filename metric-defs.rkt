#lang racket

(provide
 metric-lookup
 diagonal-metric%
 conformal-metric%
 conformal-metric-minkowski%)

;; A ProductMetric is an interface that implements
;;
;; - lookup-metric : ProductMetric (List-of Dim) (List-of Dim) -> BasisBlade
;; - store-metric-value! : ProductMetric (List-of Dim) (List-of Dim) -> BasisBlade
;;
;; The default should be 0.0

;; - make-metric : Number -> ProductMetric

;;;; Interfaces

(define metric<%>
  (interface ()
    ;; lookup : (List-of Dim) (List-of Dim) -> BasisBlade
    lookup))

;; Creates the generic method for faster utilization of the metric lookup via `send-generic`.
;; lookup is a generic method that can be used like so:
;;  (send-generic m lookup (List-of Dim) (List-of Dim))
(define lookup (generic metric<%> lookup))
(define (metric-lookup metric left right)
  (send-generic metric lookup left right))

;;;; Metrics

;; A NullMetric-over X is a Class that represents the simplest metric over an algebraic structure X.
;;  (new base-metric% [zero-el X])
;; The metric returns a zero-like element for any and all pairs of elements.
(define null-metric%
  (class* object% (metric<%>)
    (init [zero-el 0] [element-fn list])
    (super-new)

    ;; The main hash table that store the metric results
    ;; Extended by children to be non-null
    (define metric-hash (make-hash))

    ;; zero-element : X
    ;; The zero-like element that the base algebra is over
    ;; For real numbers this would be 0,
    ;; for complex, 0+0i,
    ;; etc.
    ;; These examples would all be covered by 0 though, by Scheme's numeric tower.
    ;; However, if we want more generic algebras, we need this.
    (define zero-element zero-el)

    ;; make-element : X Dims -> Y
    ;; converts a "coefficient" and a list of dimensions to an element of the algebra
    (define make-element element-fn)
    
    ;; lookup-metric : (List-of Dim) (List-of Dim) -> Number
    ;; looks up the value of a product on the metric table
    (define/public (lookup l-dims r-dims)
      (let ([l-index (dims->index l-dims)]
            [r-index (dims->index r-dims)])
        (hash-ref metric-hash (cons l-index r-index) (make-element zero-element '()))))))

;; A DiagonalMetric is a Class
;;  (new diagonal-metric% [p Int] [q Int] [r Int]
;;                        [pos-el X] [neg-el X] [zero-el X]
;;                        [element-fn (X Dims) -> Y)]
;; It represents a metric over basis elements where
;; the first p basis elements square to 1
;; the next q basis elements square to -1
;; and the final r elements square to 0
;; where 1, -1, and 0 are generalized elements of X.
(define diagonal-metric%
  (class* object% (metric<%>) 
    (init p q r [pos-el 1] [neg-el -1] [zero-el 0] [element-fn list])
    (super-new)


    ;; The main hash table that store the metric results
    ;; Extended by children to be non-null
    (define metric-hash (make-hash))

    ;; The zero-like and one-like elements in the base algebra
    ;; For real numbers this would be 0, 1, -1.
    ;; for complex, 0+0i, 1+0i, -1+0i
    ;; etc.
    ;; These examples would all be covered by 0, 1, and -1 by Scheme's numeric tower.
    ;; However, if we want more generic algebras, we need this.
    
    ;; positive-element : X
    (define positive-element pos-el)
    ;; negative-element : X
    (define negative-element neg-el)
    ;; zero-element : X
    (define zero-element zero-el)

    ;; make-element : X Dims -> Y
    ;; converts a "coefficient" and a list of dimensions to an element of the algebra
    (define make-element element-fn)

    ;; lookup-metric : (List-of Dim) (List-of Dim) -> Number
    ;; looks up the value of a product on the metric table
    (define/public (lookup l-dims r-dims)
      (let ([l-index (dims->index l-dims)]
            [r-index (dims->index r-dims)])
        (hash-ref metric-hash (cons l-index r-index) (make-element zero-element '()))))

    ;; store-metric-value! : (List-of Dim) (List-of Dim) Element -> Element
    ;; effect: stores a value in the metric table
    (define/private (store-value! l-dims r-dims val)
      (let ([l-index (dims->index l-dims)]
            [r-index (dims->index r-dims)])
        (hash-set! metric-hash (cons l-index r-index) val)))

    ;; cache-metric! : Integer AList-> Void
    ;; effect: populates the metric with the basis blade products
    (define/private (cache-metric! dimension metric-alist)
      (for* ([left (in-combinations (range 0 dimension))]
             [right (in-combinations (range 0 dimension))])
        (let-values ([(dims coef) (apply-metric left right metric-alist)])
          (store-value!
           left right
           (make-element 
            (cond [(= coef 0) zero-element]
                  [(> coef 0) positive-element]
                  [(< coef 0) negative-element]
                  [else
                   (error "Diagonal metric creation generated an invalid metric coefficient: " coef)])
            dims)))))

    (cache-metric! (+ p q r) (append (diagonal-helper p 0 1)
                                     (diagonal-helper q p -1)
                                     (diagonal-helper r (+ p q) 0)))))

;; A ConformalMetric is a Class
;;  (new conformal-metric% [d Int]
;;                         [pos-el X] [neg-el X] [zero-el X]
;;                         [element-fn (X Dims) -> Y)]
;; It represents a the conformal metrics in of d dimensions.
;; This representation uses the diagonal representation
;;      0  1  2  +  -
;;   0  1  0  0  0  0
;;   1  0  1  0  0  0
;;   2  0  0  1  0  0
;;   +  0  0  0  1  0
;;   -  0  0  0  0  -1
(define conformal-metric%
  (class diagonal-metric%
    (init [d 3] [pos-el 1] [neg-el -1] [zero-el 0] [element-fn list])
    (super-new [p (+ d 1)] [q 1] [r 0]
               [pos-el pos-el]
               [neg-el neg-el]
               [zero-el zero-el]
               [element-fn element-fn])))

;; A ConformalMiknowskiMetric is a Class
;;  (new conformal-metric-minkowski% [d Int]
;;                         [pos-el X] [neg-el X] [zero-el X]
;;                         [element-fn (X Dims) -> Y)]
;; It represents a the conformal metrics in of d dimensions.
;; This representation uses the non-diagonal representation
;;      0  1  2  o  inf
;;   0  1  0  0  0  0
;;   1  0  1  0  0  0
;;   2  0  0  1  0  0
;;   o  0  0  0  0 -1
;; inf  0  0  0 -1  0
(define conformal-metric-minkowski%
  (class* object% (metric<%>) 
    (init [d 3] [pos-el 1] [neg-el -1] [zero-el 0]
          [o-inf-el #f]
          [inf-o-el #f]
          [element-fn (lambda (x dims) (list x dims))])
    (super-new)

    ;; The main hash table that store the metric results
    ;; Extended by children to be non-null
    (define metric-hash (make-hash))

    ;; The zero-like and one-like elements in the base algebra
    ;; For real numbers this would be 0, 1, -1.
    ;; for complex, 0+0i, 1+0i, -1+0i
    ;; etc.
    ;; These examples would all be covered by 0, 1, and -1 by Scheme's numeric tower.
    ;; However, if we want more generic algebras, we need this.
    
    ;; positive-element : X
    (define positive-element pos-el)
    ;; negative-element : X
    (define negative-element neg-el)
    ;; zero-element : X
    (define zero-element zero-el)

    ;; make-element : X Dims -> Y
    ;; converts a "coefficient" and a list of dimensions to an element of the algebra
    (define make-element element-fn)

    ;; o-inf-element : Y
    ;; the element of the algebra resulting from the multiplication of o and inf
    (define o-inf-element o-inf-el)
    ;; inf-o-element: Y
    ;; the element of the algebra resulting from the multiplication of inf and o    
    (define inf-o-element inf-o-el)

    ;; lookup-metric : (List-of Dim) (List-of Dim) -> Number
    ;; looks up the value of a product on the metric table
    (define/public (lookup l-dims r-dims)
      (let ([l-index (dims->index l-dims)]
            [r-index (dims->index r-dims)])
        (hash-ref metric-hash (cons l-index r-index) (make-element zero-element '()))))

    ;; store-metric-value! : (List-of Dim) (List-of Dim) Element -> Element
    ;; effect: stores a value in the metric table
    (define/private (store-value! l-dims r-dims val)
      (let ([l-index (dims->index l-dims)]
            [r-index (dims->index r-dims)])
        (hash-set! metric-hash (cons l-index r-index) val)))

    ;; cache-metric! : Integer AList-> Void
    ;; effect: populates the metric with the basis blade products
    (define/private (cache-metric! dimension metric-alist)
      (for* ([left (in-combinations (range 0 dimension))]
             [right (in-combinations (range 0 dimension))])
        (let-values ([(dims coef) (apply-metric left right metric-alist)])
          (store-value!
           left right
           (make-element 
            (cond [(= coef 0) zero-element]
                  [(> coef 0) positive-element]
                  [(< coef 0) negative-element]
                  [else
                   (error "Diagonal metric creation generated an invalid metric coefficient: " coef)])
            dims)))))

    (cache-metric! (+ d 2) (append (diagonal-helper d 0 1)
                                   (diagonal-helper 2 d 0)))
    (let ([o-dim (list d)]
          [inf-dim (list (+ d 1))])
      (store-value! o-dim inf-dim o-inf-element)
      (store-value! inf-dim o-dim inf-o-element))))



;;;; Utilities

;; dims->index : List-of Dim -> Integer
;; converts a list of dims to an integer used for storing and retrieving the metric value
(define (dims->index dims)
  (if (empty? dims)
      0
      ;; Dimensions are mapped to 2^x
      (apply + (map (λ (x) (expt 2 x)) dims))))

;; List-of Int -> (values (List-of Int) OddEven)
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
        (values '() 0)
        (cond [(empty? dims) (values (reverse dim-acc) c)]
              [(empty? (rest dims)) (values (reverse (cons (first dims) dim-acc)) c)]
              [else
               (let* ([a (first dims)]
                      [b (second dims)]
                      [metric-val (assoc (cons a b) metric-alist)])
                 (if metric-val
                     (simplify-dims (rest (rest dims))
                                    dim-acc
                                    ;; TODO Genericize the multiplication
                                    (* c (cdr metric-val)))
                     (simplify-dims (rest dims)
                                    (cons a dim-acc)
                                    c)))])))
  (let-values ([(dims c) (simplify-dims sorted-dims empty 1)])
    (if (symbol=? parity 'even)
        (values dims c)
        ;; TODO Genericize the negation
        (values dims (- c)))))

;; Integer Integer Number -> AList
;; creates an a-list with ((index . index) . v) n times
(define (diagonal-helper n start v)
  (build-list n (λ (i)
                  (let ([idx (+ i start)])
                    (cons (cons idx idx) v)))))


    ;; ;; cache-metric! : Integer AList-> Void
    ;; ;; effect: populates the metric with the basis blade products
    ;; (define/private (cache-metric! dimension metric-alist)
    ;;   (for* ([left (in-combinations (range 0 dimension))]
    ;;          [right (in-combinations (range 0 dimension))])
    ;;     (let ([b (apply-metric left right metric-alist)])
    ;;       (store-value! left right b))))
