#lang racket

; implements naive Hamiltonian Monte Carlo

(provide hmc)

(require "leapfrog.rkt"
         "common.rkt"
         math/distributions
         math/flonum)

;; hmc-step : Parameter Flonum Integer Function Function -> Parameter
;; performs a single step of the HMC algorithm
(define (hmc-step start-theta step-size path-length log-lik grad-fn)
  (define start-momentum (draw-momentum-for start-theta))
  
  (define (mh-correction end-theta end-momentum)
    (define start-log-lik (total-energy log-lik
                                        start-theta
                                        start-momentum))
    (define end-log-lik (total-energy log-lik
                                      end-theta
                                      end-momentum))
    (define alpha (flmin 1.0 (flexp (fl- end-log-lik start-log-lik))))
    (if (< (random) alpha)
        (values end-theta end-momentum)
        (values start-theta start-momentum)))
  
  (let loop ([L path-length] [theta start-theta] [momentum start-momentum])
    (define-values (leap-theta leap-mom)
      (leapfrog theta momentum step-size grad-fn))
    (cond [(<= L 0) (mh-correction leap-theta leap-mom)]
          [else (loop (sub1 L) leap-theta leap-mom)])))
  
(define (hmc start-theta sample-size step-size path-length log-lik grad-fn)
  (for/fold ([theta start-theta]
             [samples '()]
             #:result samples)
            ([_ (in-range sample-size)])
    (let-values ([(theta _) (hmc-step theta step-size path-length log-lik grad-fn)])
      (values theta (cons theta samples)))))
