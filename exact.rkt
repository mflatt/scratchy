#lang s-exp syntax/module-reader 

#:language (lambda (p) (read-syntax (object-name p) p))
#:read (lambda (in) (parameterize ([read-decimal-as-inexact #f]) (read in)))
#:read-syntax (lambda (name in) (parameterize ([read-decimal-as-inexact #f]) (read-syntax name in)))
