#lang racket/base
(require "runtime.rkt"
         "task.rkt")

(provide forever while
         (rename-out [sync-task task])
         run+)

(define-syntax-rule (sync-task e ...)
  (task (sync ready) e ...))

(define ready-sema (make-semaphore))
(define ready (semaphore-peek-evt ready-sema))

(define (run+ l)
  (begin0
   (run l)
   (semaphore-post ready-sema)))
