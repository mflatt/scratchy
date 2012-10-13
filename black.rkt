#lang racket/base

(module reader syntax/module-reader
  #:language 'scratchy/scratchy
  #:read (lambda (in) 
           (parameterize ([read-decimal-as-inexact #f]) 
             (read in)))
  #:read-syntax (lambda (name in)
                  (parameterize ([read-decimal-as-inexact #f]) 
                    (read-syntax name in)))
  #:info (lambda (key default filter)
           (case key
             [(color-lexer)
              (define scheme-lexer (dynamic-require 'syntax-color/scheme-lexer 'scheme-lexer))
              (lambda (in)
                (define-values (text style paren start end) (scheme-lexer in))
                (define new-style (if (eq? style 'symbol)
                                      'other
                                      style))
                (values text new-style paren start end))]
             [else (filter key default)])))
