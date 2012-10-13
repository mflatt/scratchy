#lang racket/base

(provide forever while task)

(define-syntax-rule (forever expr ...)
  (let loop () expr ... (loop)))

(define-syntax-rule (while tst expr ...)
  (let loop () (when tst expr ... (loop))))

(define-syntax-rule (task expr ...)
  (void (thread (lambda () expr ... (void)))))

