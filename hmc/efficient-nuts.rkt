#lang racket

; implements the "efficient" No U-Turns Sampler without Dual Averaging

(provide eff-nuts)

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
                        stop-criterion-satisfied?)
  #:transparent)

(define DELTA-MAX 1000.0)

;; random-direction : Void -> Direction
;; generates a random direction ('forward or 'backward)
(define (random-direction)
  (if (fl< (random) 0.5) 'forward 'backward))

(define (build-forward-tree nuts-result slice-samp depth step-size log-lik grad-fn)
  (match-let ([(nuts-step-data _ _
                               forward-theta forward-momentum
                               _ _ _ ) nuts-result])
    (define tree (build-tree forward-theta forward-momentum
                             slice-samp 1.0 depth step-size log-lik grad-fn))
    (struct-copy nuts-step-data tree
                 [backward-theta (nuts-step-data-backward-theta nuts-result)]
                 [backward-momentum (nuts-step-data-backward-momentum nuts-result)])))

(define (build-backward-tree nuts-result slice-samp depth step-size log-lik grad-fn)
  (match-let ([(nuts-step-data backward-theta backward-momentum
                               _ _
                               _ _ _ ) nuts-result])
    (define tree (build-tree backward-theta backward-momentum
                             slice-samp -1.0 depth step-size log-lik grad-fn))
    (struct-copy nuts-step-data tree
                 [forward-theta (nuts-step-data-forward-theta nuts-result)]
                 [forward-momentum (nuts-step-data-forward-momentum nuts-result)])))

(define (build-tree theta momentum slice-samp direction depth step-size log-lik grad-fn)
  (define (bt-base)
    (define-values (new-theta new-momentum)
      (leapfrog theta momentum (fl* direction step-size) grad-fn))
    (define energy (total-energy log-lik new-theta new-momentum))
    (define upper-bound (flexp energy))
    (define valid-samples (if (fl<= slice-samp upper-bound) 1.0 0.0))
    (define satisfies-stopping-criterion?
      (fl>= slice-samp (flexp (fl+ DELTA-MAX energy))))
    (nuts-step-data new-theta new-momentum
                    new-theta new-momentum
                    new-theta
                    valid-samples
                    satisfies-stopping-criterion?))
  (cond [(= depth 0) (bt-base)]
        [else
         (define tree-data (build-tree theta momentum
                                       slice-samp direction (sub1 depth) step-size log-lik grad-fn))
         (define old-n (nuts-step-data-valid-samples tree-data))
         (define old-sample (nuts-step-data-current-sample tree-data))
         
         (define recur-data
           (if (= direction 1.0)
               (build-forward-tree tree-data slice-samp (sub1 depth) step-size
                                   log-lik grad-fn)
               (build-backward-tree tree-data slice-samp (sub1 depth) step-size
                                    log-lik grad-fn)))
         
         (match-let ([(nuts-step-data backward-theta backward-momentum
                                      forward-theta forward-momentum
                                      new-sample
                                      new-n
                                      new-satisfies-stopping-criterion?)
                      recur-data])

           (define total-n (+ old-n new-n))
           
           (define chosen-sample
             (if (fl< (random) (/ new-n total-n))
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
                           total-n
                           stop?))]))

(define (eff-nuts-step start-theta step-size log-lik grad-fn)
  (define start-momentum (draw-momentum-for start-theta))
  (define slice-bound (flexp (total-energy log-lik start-theta start-momentum)))
  (define slice-samp (flvector-ref (fluniform-sample 0.0 slice-bound 1) 0))
  (let loop ([step-data (nuts-step-data start-theta start-momentum
                                        start-theta start-momentum
                                        start-theta
                                        1.0
                                        #f)]
             [depth 0])

    (define old-n (nuts-step-data-valid-samples step-data))
    (define direction (random-direction))
    (define update-data 
      (match direction
        ['forward (build-forward-tree step-data slice-samp depth step-size log-lik grad-fn)]
        ['backward (build-backward-tree step-data slice-samp depth step-size log-lik grad-fn)]))
    (match-let ([(nuts-step-data backward-theta backward-momentum
                                 forward-theta forward-momentum
                                 proposed-sample
                                 new-n
                                 tree-update-stop?)
                 update-data])

      (define chosen-sample
        (if tree-update-stop?
            start-theta
            (if (< (random) (min 1.0 (fl/ new-n old-n)))
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

      (cond [stop? chosen-sample]
            [else
             (loop (nuts-step-data backward-theta backward-momentum
                                   forward-theta forward-momentum
                                   chosen-sample
                                   (+ old-n new-n)
                                   #f)
                   (add1 depth))]))))


(define (eff-nuts start-theta sample-size step-size log-lik grad-fn)
  (for/fold ([theta start-theta]
             [samples (list start-theta)]
             #:result samples)
            ([_ (in-range sample-size)])
    (let ([new-theta (eff-nuts-step theta step-size log-lik grad-fn)])
      (values new-theta (cons new-theta samples)))))


