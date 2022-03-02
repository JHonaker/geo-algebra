#lang racket

; implements the No U-Turns Sampler with Dual Averaging


;(provide eff-nuts)
(provide (all-defined-out))

(require "leapfrog.rkt"
         "common.rkt"
         (only-in racket/random random-sample)
         math/flonum
         math/distributions)

(struct nuts-step-data (backward-theta
                        backward-momentum
                        forward-theta
                        forward-momentum
                        current-sample
                        valid-samples
                        stop-criterion-satisfied?
                        alpha
                        total-samples)
  #:transparent)

(define DELTA-MAX 1000.0)

(define (find-reasonable-step-size start-theta log-lik grad-fn)
  (define max-iter 100)
  (define start-momentum (draw-momentum-for start-theta))
  (define start-energy (total-energy log-lik start-theta start-momentum))
  (define-values (theta momentum) (leapfrog start-theta start-momentum 1.0 grad-fn))
  (define lik-ratio (flexp (fl- (total-energy log-lik theta momentum)
                                  start-energy)))
  (define a (if (fl> lik-ratio 0.5) 1.0 -1.0))
  (let loop ([iter 0] [theta theta] [momentum momentum] [step-size 1.0])
    (define lik-ratio (flexp (fl- (total-energy log-lik theta momentum)
                                  start-energy)))
    (cond [(and (< iter max-iter)
                (fl> (flexpt lik-ratio a) (flexpt 2.0 a)))
           (define new-step-size (fl* (flexpt 2.0 a) step-size))
           (define-values (theta momentum)
             (leapfrog start-theta start-momentum new-step-size grad-fn))
           (loop (add1 iter) theta momentum new-step-size)]
          [else step-size])))

;; random-direction : Void -> Direction
;; generates a random direction ('forward or 'backward)
(define (random-direction)
  (if (fl< (random) 0.5) 'forward 'backward))

(define (build-forward-tree nuts-result
                            slice-samp
                            depth
                            step-size
                            theta-0 momentum-0
                            log-lik grad-fn)
  (match-let ([(nuts-step-data _ _
                               forward-theta forward-momentum
                               _ _ _ _ _) nuts-result])
    (define tree (build-tree forward-theta forward-momentum
                             slice-samp 1.0 depth step-size theta-0 momentum-0 log-lik grad-fn))
    (struct-copy nuts-step-data tree
                 [backward-theta (nuts-step-data-backward-theta nuts-result)]
                 [backward-momentum (nuts-step-data-backward-momentum nuts-result)])))

(define (build-backward-tree nuts-result
                             slice-samp
                             depth
                             step-size
                             theta-0 momentum-0
                             log-lik grad-fn)
  (match-let ([(nuts-step-data backward-theta backward-momentum
                               _ _
                               _ _ _ _ _) nuts-result])
    (define tree (build-tree backward-theta backward-momentum
                             slice-samp -1.0 depth step-size theta-0 momentum-0 log-lik grad-fn))
    (struct-copy nuts-step-data tree
                 [forward-theta (nuts-step-data-forward-theta nuts-result)]
                 [forward-momentum (nuts-step-data-forward-momentum nuts-result)])))

(define (build-tree theta momentum
                    slice-samp
                    direction
                    depth
                    step-size
                    theta-0 momentum-0
                    log-lik grad-fn)
  (define (bt-base)
    (define-values (new-theta new-momentum)
      (leapfrog theta momentum (fl* direction step-size) grad-fn))
    (define energy (total-energy log-lik new-theta new-momentum))
    (define energy-0 (total-energy log-lik theta-0 momentum-0))
    (define upper-bound (flexp energy))
    (define valid-samples (if (fl<= slice-samp upper-bound) 1.0 0.0))
    (define satisfies-stopping-criterion?
      (fl>= slice-samp (flexp (fl+ DELTA-MAX energy))))
    (define alpha (flmin 1.0 (flexp (fl- energy energy-0))))
    (nuts-step-data new-theta new-momentum
                    new-theta new-momentum
                    new-theta
                    valid-samples
                    satisfies-stopping-criterion?
                    alpha
                    1.0))
  (cond [(= depth 0) (bt-base)]
        [else
         (define tree-data (build-tree theta momentum
                                       slice-samp
                                       direction
                                       (sub1 depth)
                                       step-size
                                       theta-0 momentum-0
                                       log-lik grad-fn))
         (define old-n (nuts-step-data-valid-samples tree-data))
         (define old-sample (nuts-step-data-current-sample tree-data))
         (define old-alpha (nuts-step-data-alpha tree-data))
         (define old-total-samples (nuts-step-data-total-samples tree-data))
         
         (define recur-data
           (if (= direction 1.0)
               (build-forward-tree tree-data slice-samp (sub1 depth) step-size
                                   theta-0 momentum-0
                                   log-lik grad-fn)
               (build-backward-tree tree-data slice-samp (sub1 depth) step-size
                                    theta-0 momentum-0
                                    log-lik grad-fn)))
         
         (match-let ([(nuts-step-data backward-theta backward-momentum
                                      forward-theta forward-momentum
                                      new-sample
                                      new-n
                                      new-satisfies-stopping-criterion?
                                      new-alpha
                                      new-total-samples)
                      recur-data])
           
           (define out-alpha (fl+ new-alpha old-alpha))
           (define out-total-samples (fl+ old-total-samples new-total-samples))
           (define out-n (+ old-n new-n))
           
           (define chosen-sample
             (if (fl< (random) (/ new-n out-n))
                 new-sample
                 old-sample))

           (define theta-trajectory (flvector- forward-theta backward-theta))

           (define forward-u-turns?
             (fl< (flvector-dot theta-trajectory
                                forward-momentum)
                  0.0))

           (define backward-u-turns?
             (fl< (flvector-dot theta-trajectory
                                backward-momentum)
                  0.0))
           
           (define stop? (or new-satisfies-stopping-criterion?
                             forward-u-turns?
                             backward-u-turns?))

           (nuts-step-data backward-theta backward-momentum
                           forward-theta forward-momentum
                           chosen-sample
                           out-n
                           stop?
                           out-alpha
                           out-total-samples))]))

(define (nuts-step start-theta step-size log-lik grad-fn)
  (define start-momentum (draw-momentum-for start-theta))
  (define slice-bound (flexp (total-energy log-lik start-theta start-momentum)))
  (define slice-samp (flvector-ref (fluniform-sample 0.0 slice-bound 1) 0))
  (let loop ([step-data (nuts-step-data start-theta start-momentum
                                        start-theta start-momentum
                                        start-theta
                                        0.0 #f 0.0 0.0)]
             [depth 0]
             [n-valid 1.0])

    (define direction (random-direction))
    (define update-data 
      (match direction
        ['forward (build-forward-tree step-data
                                      slice-samp
                                      depth
                                      step-size
                                      start-theta start-momentum
                                      log-lik grad-fn)]
        ['backward (build-backward-tree
                    step-data
                    slice-samp
                    depth
                    step-size
                    start-theta start-momentum
                    log-lik grad-fn)]))
    (match-let ([(nuts-step-data backward-theta backward-momentum
                                 forward-theta forward-momentum
                                 proposed-sample
                                 new-n-valid
                                 tree-update-stop?
                                 _ _) ; These last two are passed to NUTS for use
                 update-data])

      (define chosen-sample
        (if tree-update-stop?
            start-theta
            (if (< (random) (min 1.0 (fl/ new-n-valid n-valid)))
                proposed-sample
                start-theta)))
      
      (define theta-trajectory (flvector- forward-theta
                                          backward-theta))
      (define forward-is-u-turn?
        (fl< (flvector-dot theta-trajectory
                           forward-momentum) 0.0))
      (define backward-is-u-turn?
        (fl< (flvector-dot theta-trajectory
                           backward-momentum) 0.0))

      (define stop? (or tree-update-stop?
                        forward-is-u-turn?
                        backward-is-u-turn?))

      (define step-data (struct-copy nuts-step-data update-data 
                                     [current-sample chosen-sample]))
      
      (cond [stop? step-data]
            [else
             (loop step-data
                   (add1 depth)
                   (+ n-valid new-n-valid))]))))


(struct nuts-config (shrinkage-target ; mu
                     shrinkage-strength ; gamma
                     decay-rate ; kappa
                     ))

(define (nuts start-theta sample-size adapt-steps target-acceptance-rate log-lik grad-fn)
  (define start-step-size (find-reasonable-step-size start-theta log-lik grad-fn))

  ;;; Dual averaging parameters with names from paper
  ;; target-acceptance rate ; gamma
  (define shrinkage-target (fllog (fl* 10.0 start-step-size))) ; mu
  (define shrinkage-strength 0.05) ; gamma
  (define decay-rate 0.75) ; kappa
  (define initial-strength 10.0) ; t0

  (define (compute-H-statistic step-number last-H step-data)
    (define diff-from-target-rate (nuts-step-data-alpha step-data))
    (define total-samples (nuts-step-data-total-samples step-data))
    (define dual-avg-weight (fl/ 1.0 (fl+ (fl step-number) initial-strength)))
    (fl+ (fl* (fl- 1.0 dual-avg-weight) last-H)
         (fl* dual-avg-weight (fl- target-acceptance-rate
                                   (fl/ diff-from-target-rate
                                        total-samples)))))
  
  (define (compute-log-step-size step-number H)
    (define coef (fl/ (flsqrt (fl step-number)) shrinkage-strength))
    (fl- shrinkage-target
         (fl* coef H)))

  (define (compute-log-adapt-step-size step-number log-step-size last-log-adapt-step-size)
    (define coef (flexpt (fl step-number) (fl- decay-rate)))
    (fl+ (fl* coef log-step-size)
         (fl* (fl- 1.0 coef)
              last-log-adapt-step-size)))

  (define-values (adapted-step-size adapt-samples)
    (for/fold ([step-size start-step-size]
               [log-adapt-step-size 0.0]
               [H 0.0]
               [theta start-theta]
               [adapt-samples (list start-theta)]
               #:result (values (flexp log-adapt-step-size) adapt-samples))
              ([step-number (in-inclusive-range 1 adapt-steps)])
      (let* ([step-data (nuts-step theta step-size log-lik grad-fn)]
             [H (compute-H-statistic step-number H step-data)]
             [log-step-size (compute-log-step-size step-number H)]
             [log-adapt-step-size (compute-log-adapt-step-size step-number
                                                               log-step-size
                                                               log-adapt-step-size)]
             [new-theta (nuts-step-data-current-sample step-data)])
        #;
        (when (zero? (modulo step-number 1))
          (printf "step-number: ~a/~a~%" step-number adapt-steps)
          (printf "H: ~a~%" H)
          (printf "step-size: ~a~%" step-size)
          (printf "log-adapt-step-size: ~a~%" log-adapt-step-size))
        (values (flexp log-step-size)
                log-adapt-step-size
                H
                new-theta
                (cons new-theta adapt-samples)))))
  
  (printf "Adapted step size: ~a~%" adapted-step-size)
  
  (define samples
    (for/fold ([theta (first adapt-samples)]
               [samples '()]
               #:result samples)
              ([step-number (in-range sample-size)])
      (let ([step-data (nuts-step theta adapted-step-size log-lik grad-fn)])
        (define new-theta (nuts-step-data-current-sample step-data))
        (values new-theta (cons new-theta samples)))))

  (list samples adapt-samples))




