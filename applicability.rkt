#lang racket

(require "utils.rkt")

(provide (all-defined-out))

;; An ApplicabilityAttribute is a List-of List
;; representing an OR of some per-argument ANDs.

(define (applicability? object)
  (and (list? object)
       (andmap (λ (pattern)
                (and (list? pattern)
                     (andmap procedure? pattern)))
              object)
       (or (not (pair? object))
           (let ([arity (length (car object))])
             (andmap (λ (pattern)
                      (= arity (length pattern)))
                    (cdr object))))))

(define (applicability-arity applicability)
  (if (applicability? applicability)
      (if (pair? applicability)
          (length (car applicability))
          0)
      (error "Not an applicability:" applicability)))

(define (is-applicable? applicability args)
  (ormap (λ (and-clause)
           (predicates-match? and-clause args))
         applicability))

(define (predicates-match? predicates args)
  (and (= (length predicates) (length args))
       (andmap (λ (predicate arg)
                 (predicate arg))
               predicates args)))

(define (match-args . predicates)
  (list predicates))

(define (all-args arity predicate)
  (list (make-list arity predicate)))

(define (any-args arity predicate base-predicate)
  (if (= 0 arity)
      '()
      (all-sequences-of arity base-predicate predicate)))

(define (applicability-union . applicabilities)
  (applicability-union* applicabilities))

(define (applicability-union* applicabilities)
  (apply set-union applicabilities))

