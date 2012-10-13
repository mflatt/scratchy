#lang racket/base
(require (for-syntax racket/base))

(provide key-handler)

(define-syntax (key-handler stx)
  (syntax-case stx ()
    [(_ [key expr ...] ...)
     (let ([keys (syntax->list #'(key ...))])
       (for ([k (in-list keys)])
         (unless (identifier? k)
           (raise-syntax-error #f
                               "not an identifier for a key"
                               stx
                               k)))
       #'(lambda (down? given-key)
           (when down?
             (cond
              [(is-key? given-key 'key) expr ...]
              ...))))]))

(define (is-key? given sym)
  (eq? (if (char? given)
           (string->symbol (substring (format "~s" given) 2))
           given)
       sym))
