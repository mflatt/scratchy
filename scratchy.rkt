#lang racket/base
(require "almost-scratchy.rkt"
         "variable.rkt"
         (for-syntax racket/base))

(provide (except-out (all-from-out "almost-scratchy.rkt")
                     set!)
         (rename-out [checked-set! set!]))

(define-syntax (checked-set! stx)
  (syntax-case stx ()
    [(_ target-id rhs)
     (let ([id #'target-id])
       (when (and (identifier? id)
                  (not (variable? (syntax-local-value id (lambda () #f)))))
         (raise-syntax-error #f
                             "target of assignment is not a variable"
                             stx
                             id))
       #`(set! target-id rhs))]))
