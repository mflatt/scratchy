#lang racket/base
(require "runtime.rkt"
         "task.rkt"
         "variable.rkt" ;; ignore until step 9
         racket/class)

(provide define-sprite
         run+)

(define-syntax define-sprite
  (syntax-rules (task variable)
    [(_ id clause ... [task expr ...])
     (begin
       (define-sprite id clause ...)
       (task (sync ready) expr ...))]
    [(_ id clause ... [variable var-id expr])
     (begin
       (define-sprite id clause ...)
       (define-variable var-id expr))]
    [(_ id clause ...)
     (define id (new sprite% clause ...))]))

(define ready-sema (make-semaphore))
(define ready (semaphore-peek-evt ready-sema))

(define (run+ l)
  (begin0
   (run l)
   (semaphore-post ready-sema)))
