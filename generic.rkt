#lang racket

(require "applicability.rkt")
(require "utils.rkt")

(provide (all-defined-out))

(define *generic-procedure-metadata-table* (make-weak-hasheqv))

(define (generic-procedure? key)
  (hash-has-key? *generic-procedure-metadata-table* key))

(define (generic-procedure-metadata object)
  (hash-ref *generic-procedure-metadata-table* object))

(define (set-generic-procedure-metadata! key metadata)
  (when (generic-procedure? key)
    (let ([existing-metadata (hash-ref *generic-procedure-metadata-table* key)])
      (unless (eqv? metadata existing-metadata)
        (error "Cannot change metadata for:" key metadata existing-metadata))))
  (hash-set! *generic-procedure-metadata-table* key metadata))

(define (error-generic-procedure-handler name)
  (lambda args
    (error "Inapplicable generic procedure:" name args)))

(struct generic-metadata (name arity dispatcher getter default-getter))

(define (make-generic-metadata name arity dispatcher [getter #f] [default-getter #f])
  ((dispatcher 'set-default-handler!) default-getter)
  (generic-metadata name arity dispatcher
                    (dispatcher 'get-handler)
                    (dispatcher 'get-default-handler)))

(define (generic-procedure-constructor dispatch-store-maker)
  (λ (name arity default-handler)
    (let ([metadata  (make-generic-metadata
                      name arity (dispatch-store-maker)
                      (or default-handler
                          (error-generic-procedure-handler name)))])
      (define (the-generic-procedure . args)
        (generic-procedure-dispatch metadata args))
      (set-generic-procedure-metadata! the-generic-procedure
                                       metadata)
      the-generic-procedure)))

(define (make-simple-dispatch-store)
  (let ([rules '()]
        [default-handler #f])
    (define (get-handler args)
      (let ([rule (findf (λ (rule)
                           (predicates-match? (car rule) args))
                         rules)])
        (when rule (cdr rule))))
    (define (add-handler! applicability handler)
      (for-each (λ (predicates)
                  (set! rules
                        (list-updatef
                         rules
                         (λ (p) (equal? (car p) predicates))
                         (cons predicates handler))))
                  ;; (let ([p (assoc predicates rules)])
                  ;;   (if p
                  ;;       (set-cdr! p handler)
                  ;;       (set! rules (cons (cons predicates handler)
                  ;;                         rules))))
                applicability))
    (define (get-default-handler) default-handler)
    (define (set-default-handler! handler)
      (set! default-handler handler))
    (λ (message)
      (case message
        [(get-handler) get-handler]
        [(add-handler!) add-handler!]
        [(get-default-handler) get-default-handler]
        [(set-default-handler!) set-default-handler!]
        [(get-rules) (λ () rules)]
        [else (error "Unknown message: " message)]))))

(define (define-generic-procedure-handler generic-procedure applicability handler)
  (((generic-metadata-dispatcher
     (generic-procedure-metadata generic-procedure))
    'add-handler!)
   applicability
   handler))

(define (generic-procedure-dispatch metadata args)
  (let ([handler (get-generic-procedure-handler metadata args)])
    (apply handler args)))

(define (get-generic-procedure-handler metadata args)
  (or ((generic-metadata-getter metadata) args)
      ((generic-metadata-default-getter metadata))))

(define (generic-procedure-name proc)
  (generic-metadata-name (generic-procedure-metadata proc)))

(define (generic-procedure-arity proc)
  (generic-metadata-arity (generic-procedure-metadata proc)))

(define (generic-procedure-rules proc)
  (([generic-metadata-dispatcher (generic-procedure-metadata proc)] 'get-rules)))

(define (generic-procedure-handlers proc)
  (map cdr (generic-procedure-rules proc)))

(define (assign-handler!* proc handler preds)
  (define-generic-procedure-handler
    proc
    preds
    handler))

(define (assign-handler! proc handler . preds)
  (assign-handler!* proc handler (apply match-args preds)))

(define simple-generic-procedure
  (generic-procedure-constructor make-simple-dispatch-store))

(define-syntax (define/generic stx)
  (syntax-case stx ()
    [(_ (proc-name id ...))
     #'(define/generic (proc-name id ...) #f)]
    [(_ (proc-name id ...) default-handler)
     #`(define proc-name
         (simple-generic-procedure (quote proc-name)
                                   #,(length (syntax->list #'(id ...)))
                                   default-handler))]))

(define-syntax (define/implementation stx)
  (syntax-case stx ()
    [(_ (proc-name (id pred) ...) body ...)
     #'(define-generic-procedure-handler proc-name (match-args pred ...)
       (lambda (id ...)
         body ...))]))
