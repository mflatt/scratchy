#lang racket/base

(module reader syntax/module-reader
  #:language 'scratchy/scratchy
  #:read (lambda (in) 
           (parameterize ([read-decimal-as-inexact #f]) 
             (read in)))
  #:read-syntax (lambda (name in)
                  (parameterize ([read-decimal-as-inexact #f]) 
                    (read-syntax name in))))

  
;; Equivalent, more primitive:
#;
(module reader racket/base
  (provide (rename-out [s-read-syntax read-syntax]
                       [s-read read]))

  (define (s-read-syntax src-name in)
    (define stxes (let loop ()
                    (define v (read-syntax src-name in))
                    (if (eof-object? v)
                        null
                        (cons v (loop)))))
    (datum->syntax #f `(module prog scratchy/scratchy
                         (#%module-begin . ,stxes))))

  (define (s-read in)
    (syntax->datum (s-read-syntax (object-name in) in))))
