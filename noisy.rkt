#lang racket/base

(module reader syntax/module-reader
  #:language 'scratchy/scratchy
  #:read (lambda (in) 
           (parameterize ([read-decimal-as-inexact #f]) 
             (read in)))
  #:read-syntax (lambda (name in)
                  (parameterize ([read-decimal-as-inexact #f]) 
                    (read-syntax name in)))
  #:language-info '#((submod scratchy/noisy language-info) get-get-info #f))

(module language-info racket/base
  (provide get-get-info)
  (define (get-get-info v)
    (lambda (key default)
      (case key
        [(configure-runtime) '(#((submod scratchy/noisy init) init #f))]
        [else default]))))

(module init racket/base
  (provide init)
  (define (init v)
    (thread (lambda ()
              (define r (make-log-receiver (current-logger) 'debug))
              (let loop ()
                (define v (sync r))
                (printf "~a\n" (vector-ref v 1))
                (loop))))))
