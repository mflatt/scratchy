#lang racket/base
(require (for-syntax racket/base
                     syntax/parse))

(provide define-variable
         (for-syntax variable?))

(begin-for-syntax
 (struct variable (id)
   #:property 
   prop:set!-transformer
   (lambda (self stx)
     (with-syntax ([id (variable-id self)])
       (syntax-parse stx #:literals (set!)
         [(set! _ rhs) (syntax/loc stx (set! id rhs))]
         [(_ arg ...) (syntax/loc stx (id arg ...))]
         [_ #'id])))))

(define-syntax (define-variable stx)
  (syntax-parse stx
    [(_ id rhs)
     #'(begin
         (define-syntax id (variable #'gen-id))
         (define gen-id (let ([id rhs]) id)))]))
