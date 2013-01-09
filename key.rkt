#lang racket/base
(require (for-syntax racket/base
                     syntax/parse))

(provide key-handler)

;; Ok, but doesn't ensure that a `key' is an identifier:
#;
(define-syntax-rule (key-handler [key expr ...] ...)
  (lambda (down? given-key)
    (when down?
      (cond
       [(is-key? given-key 'key) expr ...]
       ...))))

;; Better syntax checking:
(define-syntax (key-handler stx)
  (syntax-parse stx
    [(_ [key expr ...] ...)
     (define keys (syntax->list #'(key ...)))
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
            ...)))]))

;; is-key? : (or/c char? symbol?) symbol? -> boolean?
;;  Determines whether the given "key" from the Racket GUI layer
;;  matches out simplified symbol representation of keys
(define (is-key? given sym)
  (eq? (if (char? given)
           (string->symbol (substring (format "~s" given) 2))
           given)
       sym))
