#lang s-exp syntax/module-reader 

#:language (lambda (p) 
             (read-syntax (object-name p) p))
#:read read/exact
#:read-syntax read-syntax/exact

(require "read-exact.rkt")

#|
;; The primitive way:
#lang racket/base
(require "read-exact.rkt")
(provide (rename-out [read* read]
                     [read-syntax* read-syntax]))

(define (read* in)
  (syntax->datum (read-syntax* (object-name in) in)))

(define (read-syntax* src in)
  (define es
    (for/list ([e (in-producer (lambda () (read-syntax/exact src in)) eof)])
      e))
  #`(module prog #,(car es)
      . #,(cdr es)))
|#

