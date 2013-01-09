#lang racket/base
(require "runtime.rkt"
         "images.rkt"
         "sync-task.rkt"
         "key.rkt"
         "kw-sprite.rkt")

(provide (except-out (all-from-out racket/base)
                     #%module-begin)
         (rename-out [module-begin #%module-begin])

         (all-from-out "images.rkt"
                       "sync-task.rkt"
                       "key.rkt"
                       "kw-sprite.rkt"))

(define-syntax module-begin 
  (syntax-rules (define-sprite)
    [(_ (define-sprite id . rest) ...)
     (#%module-begin
      (define-sprite id . rest) ...
      (run+ (reverse (list id ...))))]))
