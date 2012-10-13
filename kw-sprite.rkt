#lang racket/base
(require "runtime.rkt"
         "images.rkt"
         "task.rkt"
         "key.rkt"
         (rename-in "method.rkt" 
                    [define-sprite define-sprite/orig])
         (for-syntax syntax/parse
                     racket/base))

(provide (except-out (all-from-out "method.rkt")
                     define-sprite/orig)
         define-sprite)

(define-syntax (define-sprite stx)
  (syntax-parse stx
    [(_ name:id (~or (~once (~seq #:image image-expr:expr))
                     (~optional (~seq #:x x-expr:expr)
                                #:defaults ([x-expr #'0]))
                     (~optional (~seq #:y y-expr:expr)
                                #:defaults ([y-expr #'0]))
                     (~optional (~seq #:size size-expr:expr)
                                #:defaults ([size-expr #'1]))
                     (~optional (~seq #:direction dir-expr:expr)
                                #:defaults ([dir-expr #'90]))
                     (~seq #:key key-name:id key-expr:expr ...)
                     (~seq #:task task-expr:expr ...)
                     (~seq #:variable variable-name:id variable-expr:expr))
        ...)
     #'(define-sprite/orig name
         [image image-expr]
         [x x-expr]
         [y y-expr]
         [size size-expr]
         [direction dir-expr]
         [key-callback
          (key-handler
           [key-name key-expr ...]
           ...)]
         [variable variable-name variable-expr] ...
         [task task-expr ...] ...)]))
