#lang racket/base
(require "runtime.rkt"
         "images.rkt"
         racket/stxparam
         racket/splicing
         racket/class
         (for-syntax racket/base
                     syntax/parse))

(provide (except-out (all-from-out racket/base)
                     #%module-begin set!)
         (rename-out [module-begin #%module-begin]
                     [checked-set! set!])

         (all-from-out "images.rkt")

         define-sprite task

         forever while)

;; The methods listed here are also exported:
(define-syntax-rule (provide-methods)
  (provide-method move-y move-x turn-to change-size
                  forward turn touches? say hush))

;; ----------------------------------------

(define-syntax module-begin 
  (syntax-rules (define-sprite)
    [(_ (define-sprite id . rest) ...)
     (#%module-begin
      (define-sprite id . rest) ...
      (run+ (reverse (list id ...))))]))

;; ----------------------------------------

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
     #'(define-sprite/method name
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

;; ----------------------------------------

(define-syntax-parameter self #f)

(define-syntax-rule (provide-method id ...)
  (begin
    (provide id ...)
    (define-method id ...)))

(define-syntax-rule (define-method id ...)
  (begin
    (...
     (define-syntax-rule (id arg ...)
       (send (self) id arg ...)))
    ...))

(define-method move-y move-x turn-to change-size
  forward turn touches? say hush)

(define-syntax-rule (define-sprite/method name clause ...)
  (splicing-syntax-parameterize ([self (syntax-rules ()
                                         [(_) name])])
    (define-sprite/orig name clause ...)))

(provide-methods)

;; ----------------------------------------

(define task-custodian (make-custodian))

(define ready-sema (make-semaphore))
(define ready (semaphore-peek-evt ready-sema))

(define (run+ l)
  (begin0
   (run l #:on-close (lambda () (custodian-shutdown-all task-custodian)))
   (semaphore-post ready-sema)))

(define-syntax define-sprite/orig
  (syntax-rules (task variable)
    [(_ id clause ... [task expr ...])
     (begin
       (define-sprite/orig id clause ...)
       (task (sync ready) expr ...))]
    [(_ id clause ... [variable var-id expr])
     (begin
       (define-sprite/orig id clause ...)
       (define-variable var-id expr))]
    [(_ id clause ...)
     (define id (new sprite% clause ...))]))

;; ----------------------------------------

(define-syntax (checked-set! stx)
  (syntax-case stx ()
    [(_ target-id rhs)
     (let ([id #'target-id])
       (when (and (identifier? id)
                  (not (variable? (syntax-local-value id (lambda () #f)))))
         (raise-syntax-error #f
                             "target of assignment is not a variable"
                             stx
                             id))
       #`(set! target-id rhs))]))

(begin-for-syntax
 (struct variable (id)
   #:property 
   prop:set!-transformer
   (lambda (self stx)
     (with-syntax ([id (variable-id self)])
       (syntax-case stx (set!)
         [(set! _ rhs) (syntax/loc stx (set! id rhs))]
         [(_ arg ...) (syntax/loc stx (id arg ...))]
         [_ #'id])))))

(define-syntax (define-variable stx)
  (syntax-case stx ()
    [(_ id rhs)
     #'(begin
         (define-syntax id (variable #'gen-id))
         (define gen-id (let ([id rhs]) id)))]))

;; ----------------------------------------

(define-syntax-rule (forever expr ...)
  (let loop () expr ... (loop)))

(define-syntax-rule (while tst expr ...)
  (let loop () (when tst expr ... (loop))))

(define-syntax-rule (task expr ...)
  (void (parameterize ([current-custodian task-custodian])
          (thread (lambda () expr ... (void))))))

;; ----------------------------------------

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
