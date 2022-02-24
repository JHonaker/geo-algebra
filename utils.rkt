#lang racket

(provide (all-defined-out))

(define (list-updatef lst pred val)
  (cond [(empty? lst) (cons val empty)]
        [(pred (first lst)) (cons val (rest lst))]
        [else (cons (first lst) (list-updatef (rest lst) pred val))]))

(define (all-sequences-of arity zero one)
  (map (lambda (index)
         (index->choices index arity zero one))
       (range (expt 2 arity))))

(define (index->choices index arity zero one)
  (let loop ((i 0) (index index) (choices '()))
    (if (< i arity)
        (loop (+ i 1)
              (quotient index 2)
              (cons (if (odd? index) one zero)
                    choices))
        choices)))
