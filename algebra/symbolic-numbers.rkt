#lang racket

; defines the algebraic structure of the field of symbols

(provide symbolic-numbers%
         define-symbolic-algebra)

(require "field.rkt")
(require (only-in "../utils.rkt"
                  approx-zero?
                  approx-one?))

(define symbolic-numbers%
  (class* object% (field<%>)
    (super-new)

    (field [base-field this])

    ;; algebra<%> implementation
    (define/public (a:element-of? val)
      (or (list? val)
          (symbol? val)
          (number? val)))

    (define/public (a:algebra-of _)
      this)

    (define/public (a:el-ref x . xs)
      (cond [(list? x) (apply list-ref x xs)]
            [else x]))

    (define/private (->symbolic x)
      (cond [(list? x) (map (λ (x) (->symbolic x)) x)]
            [(symbol? x) x]
            [(number? x)
             (cond [(approx-zero? x) 0]
                   [(approx-one? x) 1]
                   [else x])]
            [else (error '->symbolic "Unexpected symbolic value: ~a~%" x)]))

    (define/public (a:make-element . xs)
      (->symbolic xs))

    (define/public (a:base->element val)
      (->symbolic val))

    (define/public (a:element->base val)
      val)

    (define/public (a:->element val)
      (->symbolic val))
    
    (define/public (a:= x y)
      (equal? x y))

    ;; field<%> implementation

    (define/public (f:zero-element)
      0)

    (define/public (f:one-element)
      1)

    (define/public (f:+ left right)
      (list '+ left right))

    (define/public (f:- left right)
      (list '- left right))

    (define/public (f:* left right)
      (list '* left right))

    (define/public (f:/ left right)
      (list '/ left right))

    (define/public (f:+-inverse x)
      (list 'neg x))

    (define/public (f:*-inverse x)
      (list 'inverse x))

    (define/public (f:scale x y)
      (list 'scale x y))
    ))

(define-syntax-rule (define-symbolic-algebra name)
  (begin
    (define name (new symbolic-numbers%))
    (install-field-operations! name)))

