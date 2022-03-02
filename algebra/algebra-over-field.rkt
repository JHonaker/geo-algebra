#lang racket

; defines the algebraic structure of an algebra over a field

(require "algebra.rkt")

(define algebra-over-field
  (interface (algebra<%>)
    aof:+
    aof:*
    aof:scale))



