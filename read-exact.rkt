#lang racket/base
(provide read/exact
         read-syntax/exact)

(define (read/exact in)
  (parameterize ([read-decimal-as-inexact #f])
    (read in)))

(define (read-syntax/exact src in)
  (parameterize ([read-decimal-as-inexact #f])
    (read in)))
