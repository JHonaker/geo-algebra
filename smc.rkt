#lang racket

(require racket/random)
(require math/flonum)
(require math/distributions)
(require flomat)

(struct atomic-type (name p-self-root))
;; An AtomicType is a struct:
;;   (atomic-type Symbol AtomicConstructor Flonum)
;; iterpretation: (atomic-type n ic p) describes an atomic type
;; whose name is n, the constructs instances using ic, and has a
;; probability of being in a scene "self-rooted" of p.

;; An AtomicConstructor-for X is a Function
;; with type Value-of X -> Instance-of X

(struct atomic-inst (type attr))
;; An AtomicInstance-of (AtomicType X) is a struct:
;;  (atomic-inst AtomicType Any)
;; interpretation: (atomic-inst t a) is an instance of an atomic type t
;; with attribute values a.


(struct composite-type (name lpdf p-self-root))
;; A CompositeType is a struct
;;  (composite-type Symbol Distribution ConstructorFn Flonum)
;; interpretation: (composite-type n l ic p) is a description of a composite type
;; with known structure
;; whose name is n, whose distribution of children is l,
;; that can create instances with ic, and whose "self-rooted"
;; probability is p

;; CompositeType . List-of Instance -> CompositeInstance
;; creates a composite instance from the child instances
(define (compose-instances type . instances)
  ((constructor type) instances))

(struct composite-inst (type children))
;; A CompositeInstance is a struct
;;   (composite-inst CompositeType (List-of Instance))
;; interpretation: (composite-inst t v c) is an instance of a composite type t
;; whose child instances are c.

;; A Type is one of: 
;; - AtomicType
;; - CompositeType

;; Type -> Bool
;; determines if x is a Type
(define (type? x)
  (or (atomic-type? x)
      (composite-type? x)))

;; (U Type Instance) -> Symbol
;; returns the name of the type or instance
(define (type-name x)
  (cond [(type? x)
         (if (atomic-type? x)
             (atomic-type-name x)
             (composite-type-name x))]
        [(instance? x) (type-name (type-of x))]
        [else (error 'type-name "Unexpected input ~a" x)]))

;; (U Type Instance) -> Number
;; returns the probability of being in a scene self-rooted
(define (self-rooted-prob x)
  (cond [(type? x)
         (if (atomic-type? x)
             (atomic-type-p-self-root x)
             (composite-type-p-self-root x))]
        [(instance? x)
         (self-rooted-prob (type-of x))]))

;; (U Type Instance) -> Number
;; returns the log-likelihood ratio of P(self-rooted) / (1 - P(self-rooted))
(define (self-rooted-loglik-ratio x)
  (let* ([p (self-rooted-prob x)]
         [logp (fllog p)]
         [log1mp (fllog (fl- 1.0 p))])
    (fl- logp log1mp)))

;; AtomicType -> ConstructorFn
;; returns the constructor function for an atomic instance
(define (make-atomic-constructor type)
  (λ vals
    (atomic-inst type vals)))

;; CompositeType -> ConstructorFn
;; returns the constructor function for a composite instance
(define (make-composite-constructor type)
  (λ chs
    (composite-inst type chs)))

;; Type -> ConstructorFn
;; returns the constructor function for a type
(define (constructor type)
  (cond [(atomic-type? type) (make-atomic-constructor type)]
        [(composite-type? type) (make-composite-constructor type)]
        [else (error 'constructor "unexpected argument ~a" type)]))

;; [X] : Type X -> Instance
;; creates an instance of type with children or values xs
(define (instantiate type . xs)
  ((constructor type) xs))

;; An Instance is one of:
;; - AtomicInstance
;; - CompositeInstance

;; Instance -> Bool
;; determines if x is an instance
(define (instance? x)
  (or (atomic-inst? x)
      (composite-inst? x)))

;; Instance -> Type
;; determines the type of the instance
(define (type-of i)
  (cond [(atomic-inst? i) (atomic-inst-type i)]
        [(composite-inst? i) (composite-inst-type i)]
        [else (error 'type-of "~a is not a valid instance." i)]))

;; Instance -> Values-of (type-of Instance)
;; returns the attribute values of an instance.
;; If the instance is composite, returns a flatten list of
;; attributes in order of traversal
(define (attr i)
  (cond [(atomic-inst? i) (atomic-inst-attr i)]
        [(composite-inst? i) (flatten (map attr (children i)))]
        [else (error 'attr-values "~a is not a valid instance." i)]))

;; Instance -> List-of Instance
;; return the child instances of an instances
(define (children i)
  (cond [(atomic-inst? i) empty]
        [(composite-inst? i) (composite-inst-children i)]
        [else (error 'children "~a is not a valid instance." i)]))

;;; Particle Methods

(struct particle (value weight))
;; A Particle-of X is a structure
;;   (particle X Flonum)
;; interpretation: (particle v w) is a SMC particle with value v and weight w

;; [X : Instance] Particle-of X -> Values-of X
;; returns the attributes of the particle
(define (particle-attr p)
  (attr (particle-value p)))

;; A ParticleSet-of X is a List-of (Particle-of X)
;; interpretation: A ParticleSet is a weighted set of values that represents a sample
;; from a particular distribution

;; [X : Instance] ParticleSet-of X -> List-of X
;; returns a list of the particles in a particle set
(define (pset-particles ps)
  (map particle-value ps))

;; [X : Instance] ParticleSet-of X -> List-of (Values-of X)
;; returns a list of the attributes of the particles in a particle set
(define (pset-attrs ps)
  (map particle-attr ps))

;; [X : Instance] ParticleSet-of X -> Type
;; returns the particle type of the particles in a particle set
(define (pset-type ps)
  (type-of (particle-value (first ps))))

;; (U CompositeInstance Particle) -> Number
;; returns the evaluated log-pdf of an instance
(define (lpdf-of x)
  (cond [(atomic-inst? x) 0.0] ;; TODO Is this fine?
        [(composite-inst? x)
         ((composite-type-lpdf (type-of x)) (attr x))]
        [(particle? x)
         (lpdf-of (particle-value x))]))

;; [X : Instance] ParticleSet-of X -> List-of Flonum
;; returns a list of the weights of the particles in a particle set
(define (pset-weights ps)
  (map particle-weight ps))

;; [X : Instance] ParticleSet-of X -> List-of Flonum
;; returns a list of normalized weights on the probability scale.
(define (pset-normalized-weights ps)
  (let* ([ws (pset-weights ps)]
         [log-normalizing-const (lgsum ws)])
    (map (λ (x) (flexp (fl- x log-normalizing-const))) ws)))

;; ParticleSet-of X -> ParticleSet-of X
;; resamples a weighted particle set that represents a sample from a particular distribution
;; returns an evelny weighted particle set from the same distribution
(define (resample ps)
  (let* ([N (length ps)]
         [new-weight (fl- (fllog (fl N)))]
         [new-particles (sample (discrete-dist (pset-particles ps)
                                               (pset-normalized-weights ps)) N)])
    (map (λ (p) (particle p new-weight)) new-particles)))


;; A Proposal is a Function that takes a specific number of particle sets and
;; returns a Function that takes a number and returns that many WeightedSamples
;; Its signature is [X : ParticleSet+] : X -> [Integer -> WeightedSample-of X]

(struct weighted-sample (value weight))
;; A WeightedSample is a structure
;;  (weighted-sample List Number)
;; interpretation: represents a weighted sample from a distribution

;; [X Y] (List-of X) (List-of Y) -> List-of '(X Y Number)
;; samples an X and Y jointly from a uniform product proposal
(define (uniform-product-proposal xs ys)
  (let* ([xs (resample xs)]
         [ys (resample ys)]
         [weight (fl- (fl+ (fllog (fl (length xs)))
                           (fllog (fl (length ys)))))]
         [x-dist (discrete-dist (pset-particles xs))]
         [y-dist (discrete-dist (pset-particles ys))])
    (λ (n)
      (let ([x-samples (sample x-dist n)]
            [y-samples (sample y-dist n)])
        (map (λ (x y) (weighted-sample (list x y) weight)) x-samples y-samples)))))

;; [Z : CompositeType] Z (ParticleSet-of X) (ParticleSet-of Y) -> (ParticleSet-of Z)
;; merges two particle sets x and y into a particle set for type merge-type
(define (pset-merge merge-type x y [n-particles #f])
  (define type-constructor (constructor merge-type))
  ;; WeightedSample -> Particle-of Z
  ;; turns a sample from a proposal into a properly weighted composite particle  
  (define (process-sample s)
    (let* ([child-instances (weighted-sample-value s)]
           [proposal-weight (weighted-sample-weight s)]
           [new-particle (type-constructor (first child-instances)
                                           (second child-instances))])
      (particle new-particle 
                (flsum (list*  (fl- (lpdf-of new-particle) proposal-weight)
                               (self-rooted-prob merge-type)
                               (map (λ (c) (fl- (self-rooted-loglik-ratio c)))
                                    child-instances))))))
  (let ([N (if n-particles
               n-particles
               (length x))]
        [proposal-sampler (uniform-product-proposal x y)])
    (map process-sample
         (proposal-sampler N))))

;; List-of Number List-of Number Number -> [Number Number -> Number]
;; returns a function that evaluate the Gaussian log pdf.
(define (mvnorm mu sig cor)
  (match-let* ([(list mux muy) mu]
               [(list sigx sigy) sig])
    (let* ([sigxx (fl* sigx sigx)]
           [sigxy (fl* sigx sigy cor)]
           [sigyy (fl* sigy sigy)]
           [Sigma (flomat: [[sigxx sigxy]
                            [sigxy sigyy]])]
           [Sig-inv (inv Sigma)]
           [Mu (flomat: [[mux] [muy]])]
           [logdet (fllog (det Sigma))]
           [log2pi (fllog (fl* 2.0 pi))]
           [const-term (fl+ (fl* -0.5 log2pi)
                            (fl* -0.5 logdet))])
      (λ (args)
        (match-let ([(list x y) args])
          (let* ([X (flomat: [[x] [y]])]
                 [r (minus X Mu)]
                 [quad-form  (flomat->flonum (times (transpose r)
                                                    Sig-inv
                                                    r))])
            (fl+ const-term
                 (fl* -0.5
                      quad-form))))))))

(define (flomat->flonum x)
  (fl (ref x 0 0)))

(define A (atomic-type 'A 0.1))
(define B (atomic-type 'B 0.1))
(define C (composite-type 'C (mvnorm (list 1.0 1.0)
                                     (list 0.5 0.5)
                                     0.8)
                          0.01))

(define N 1000)
(define As (map (λ (x) (particle (instantiate A x) 1.0))
                (build-list N (λ (_) (fl (* 2 (random)))))))
(define Bs (map (λ (x) (particle (instantiate B x) 1.0))
                (build-list N (λ (_) (fl (* 2 (random)))))))
(define Cs (pset-merge C As Bs))
(define resampled-Cs (resample Cs))


(map (λ (p) (lpdf-of (particle-value p))) resampled-Cs)

(require plot)
(list
 (plot (points (pset-attrs Cs))
       #:x-min 0 #:x-max 2
       #:y-min 0 #:y-max 2)
 (plot (points (pset-attrs resampled-Cs))
       #:x-min 0 #:x-max 2
       #:y-min 0 #:y-max 2))
