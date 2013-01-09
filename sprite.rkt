#lang racket/base
(require "runtime.rkt"
         "sync-task.rkt"
         "variable.rkt" ;; ignore until step 8
         racket/class)

(provide define-sprite)

(define-syntax define-sprite
  (syntax-rules (task variable)
    [(_ id clause ... [task expr ...])
     (begin
       (define-sprite id clause ...)
       (task expr ...))]
    [(_ id clause ... [variable var-id expr])
     (begin
       (define-sprite id clause ...)
       (define-variable var-id expr))]
    [(_ id clause ...)
     (define id (new sprite% clause ...))]))
