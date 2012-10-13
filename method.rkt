#lang racket/base
(require "runtime.rkt"
         racket/stxparam
         racket/splicing
         racket/class
         (rename-in "sprite.rkt" 
                    [define-sprite define-sprite/orig])
         (for-syntax racket/base))

(provide define-sprite run+
         
         move-y move-x turn-to change-size
         forward turn touches? say hush)

(define-syntax-parameter self #f)

(define-syntax-rule (define-method id ...)
  (begin
    (...
     (define-syntax-rule (id arg ...)
       (send (self) id arg ...)))
    ...))

(define-method move-y move-x turn-to change-size
  forward turn touches? say hush)

(define-syntax-rule (define-sprite name clause ...)
  (splicing-syntax-parameterize ([self (syntax-rules ()
                                         [(_) name])])
    (define-sprite/orig name clause ...)))
