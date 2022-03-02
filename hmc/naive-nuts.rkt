#lang racket

; implements the naive No U-Turns Sampler

(provide naive-nuts)

(require "leapfrog.rkt"
         "common.rkt"
         (only-in racket/random random-sample)
         math/flonum
         math/distributions)

(struct nuts-step-data (backward-theta
                        backward-momentum
                        forward-theta
                        forward-momentum
                        candidate-set
                        stop-criterion-satisfied?)
  #:transparent)

(define DELTA-MAX 1000.0)

;; random-direction : Void -> Direction
;; generates a random direction ('forward or 'backward)
(define (random-direction)
  (if (fl< (random) 0.5) 'forward 'backward))

(define (build-forward-tree nuts-result slice-samp depth step-size log-lik grad-fn)
  (match-let ([(nuts-step-data _ _ forward-theta forward-momentum _ _ ) nuts-result])
    (define tree (build-tree forward-theta forward-momentum
                             slice-samp 1.0 depth step-size log-lik grad-fn))
    (struct-copy nuts-step-data tree
                 [backward-theta (nuts-step-data-backward-theta nuts-result)]
                 [backward-momentum (nuts-step-data-backward-momentum nuts-result)])))

(define (build-backward-tree nuts-result slice-samp depth step-size log-lik grad-fn)
  (match-let ([(nuts-step-data backward-theta backward-momentum _ _ _ _ ) nuts-result])
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
    (define candidate-set (if (fl<= slice-samp upper-bound)
                              (set (cons new-theta new-momentum))
                              (set)))
    (define satisfies-stopping-criterion?
      (fl>= slice-samp (flexp (fl+ DELTA-MAX energy))))
    (nuts-step-data new-theta new-momentum
                    new-theta new-momentum
                    candidate-set
                    satisfies-stopping-criterion?))
  (cond [(= depth 0) (bt-base)]
        [else
         (define tree-data (build-tree theta momentum
                                       slice-samp direction (sub1 depth) step-size log-lik grad-fn))
         (define recur-data
           (if (= direction 1.0)
               (build-forward-tree tree-data slice-samp (sub1 depth) step-size
                                   log-lik grad-fn)
               (build-backward-tree tree-data slice-samp (sub1 depth) step-size
                                    log-lik grad-fn)))
         (match-let ([(nuts-step-data backward-theta backward-momentum
                                      forward-theta forward-momentum
                                      candidate-set
                                      satisfies-stopping-criterion?)
                      recur-data])

           (define theta-trajectory (flvector- forward-theta backward-theta))

           (define forward-u-turns?
             (fl< (flvector-dot theta-trajectory
                                forward-momentum)
                  0.0))

           (define backward-u-turns?
             (fl< (flvector-dot theta-trajectory
                                backward-momentum)
                  0.0))
           
           (define stop? (or (nuts-step-data-stop-criterion-satisfied? tree-data)
                             satisfies-stopping-criterion?
                             forward-u-turns?
                             backward-u-turns?))

           (nuts-step-data backward-theta backward-momentum
                           forward-theta forward-momentum
                           (set-union (nuts-step-data-candidate-set tree-data)
                                      candidate-set)
                           stop?))]))

(define (naive-nuts-step start-theta step-size log-lik grad-fn)
  (define start-momentum (draw-momentum-for start-theta))
  (define slice-bound (flexp (total-energy log-lik start-theta start-momentum)))
  (define slice-samp (flvector-ref (fluniform-sample 0.0 slice-bound 1) 0))
  (let loop ([step-data (nuts-step-data start-theta start-momentum
                                        start-theta start-momentum
                                        (set (cons start-theta start-momentum))
                                        #f)]
             [depth 0])

    (define direction (random-direction))
    (define update-data 
      (match direction
        ['forward (build-forward-tree step-data slice-samp depth step-size log-lik grad-fn)]
        ['backward (build-backward-tree step-data slice-samp depth step-size log-lik grad-fn)]))
    (match-let ([(nuts-step-data backward-theta backward-momentum
                                 forward-theta forward-momentum
                                 new-candidate-set
                                 tree-update-stop?)
                 update-data])
      (define theta-trajectory (flvector- forward-theta
                                          backward-theta))
      (define forward-is-u-turn?
        (fl< (flvector-dot theta-trajectory
                           forward-momentum) 0.0))
      (define backward-is-u-turn?
        (fl< (flvector-dot theta-trajectory
                           backward-momentum) 0.0))

      (define u-turn? (or forward-is-u-turn?
                          backward-is-u-turn?))

      (cond [tree-update-stop?
             ;; If the new tree says to stop use the /OLD/ candidate set
             (->values (random-sample (nuts-step-data-candidate-set step-data) 1))]
            [u-turn?
             ;; If we are u-turning, sample the full candidate set and return
             (->values (random-sample (set-union (nuts-step-data-candidate-set step-data)
                                                 new-candidate-set) 1))]
            [else
             ;; Otherwise, join the sets and keep going
             (loop (nuts-step-data backward-theta backward-momentum
                                   forward-theta forward-momentum
                                   (set-union (nuts-step-data-candidate-set step-data)
                                              new-candidate-set)
                                   #f)
                   (add1 depth))]))))


(define (naive-nuts start-theta sample-size step-size log-lik grad-fn)
  (for/fold ([theta start-theta]
             [samples (list start-theta)]
             #:result samples)
            ([_ (in-range sample-size)])
    (match-let ([(cons new-theta new-momentum) (naive-nuts-step theta step-size log-lik grad-fn)])
      (values new-theta (cons new-theta samples)))))

