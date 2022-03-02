#lang racket

; defines the algebraic structure of the field of real numbers

(provide real-numbers%)

(require "field.rkt"
         "ordered.rkt")

(define real-numbers%
  (class* object% (field<%> ordered<%>)
    (super-new)

    (field [base-field this])

    ;; algebra<%> implementation
    (define/public (a:base->element val)
      val)
    
    (define/public (a:element->base val)
      val)
    
    (define/public (a:->element val)
      val)
    
    (define/public (a:element-of? val)
      (number? val))

    (define/public (a:algebra-of _)
      this)

    (define/public (a:el-ref x . _)
      x)

    (define/public (a:make-element x . _)
      x)

    (define/public (a:= x y)
      (= x y))

    ;; ordered<%> implementation

    (define/public (o:< x y)
      (< x y))

    (define/public (o:> x y)
      (> x y))

    ;; field<%> implementation

    (define/public (f:zero-element)
      0.0)
    
    (define/public (f:one-element)
      1.0)

    (define/public (f:+ left right)
      (+ left right))
    
    (define/public (f:- left right)
      (- left right))
    
    (define/public (f:* left right)
      (* left right))
    
    (define/public (f:/ left right)
      (/ left right))
    
    (define/public (f:+-inverse x)
      (- x))
    
    (define/public (f:*-inverse x)
      (/ 1 x))
    
    (define/public (f:scale x y)
      (* x y))
    ))
