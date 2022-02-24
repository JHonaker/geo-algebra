#lang racket

(require "generic.rkt")
(require "applicability.rkt")

;;; Basic function tests

(define g:+ (simple-generic-procedure 'g:+ 2 #f))
(define g:- (simple-generic-procedure 'g:- 2 #f))

(define (sym:+ x y)
  (list '+ x y))

(define (sym:- x y)
  (list '- x y))

(assign-handler!* g:+ sym:+ (any-args 2 symbol? number?))
(assign-handler!* g:- sym:- (any-args 2 symbol? number?))
(assign-handler! g:+ + number? number?)
(assign-handler! g:- - number? number?)

;;; Macro interface tests

(define/generic (g:* x y))

(define/implementation (g:* [x number?] [y number?])
  (* x y))

(define/implementation (g:* (x string?) (y string?))
  (string-append x y))

;;; SFD Tests

(define foo
  (simple-generic-procedure 'foo 2 #f))

(define-generic-procedure-handler foo (match-args number? number?)
  (lambda (a b)
    (+ a b)))

(define-generic-procedure-handler foo (any-args 2 symbol? number?)
  (lambda (a b)
    (list '+ a b)))
