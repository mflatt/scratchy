#lang racket/base
(require "runtime.rkt"
         "images.rkt"
         racket/stxparam
         racket/splicing
         racket/class
         racket/runtime-path
         racket/include
         racket/promise
         (for-syntax racket/base
                     syntax/parse))

(provide (except-out (all-from-out racket/base)
                     #%module-begin set! do)
         (rename-out [module-begin #%module-begin]
                     [checked-set! set!]
                     [do-keyword do])

         (all-from-out "images.rkt")

         define-sprite task use

         forever while)

;; The methods listed here are also exported:
(define-syntax-rule (provide-methods)
  (begin
    (provide-sprite-method move-y move-x set-x set-y
                           turn-to change-size
                           forward turn touches? say hush
                           tell broadcast set-land)
    (provide-land-method watch)))

;; The additional keywords listed here are also exported:
(define-syntax-rule (provide-keywords)
  (provide-keyword image is to by on x y size direction ---...-
                   key message variable touches move change
                   wait everyone send))

;; ----------------------------------------

(define-syntax (module-begin  stx)
  (syntax-case stx ()
    [(mb decl ...)
     (with-syntax ([whole stx]
                   [land-id (datum->syntax stx
                                           (or (syntax-property stx 'enclosing-module-name)
                                               'anonymous)
                                           stx)])
       #'(#%module-begin
          (top land-id () decl ...)))]))

(define-syntax top
  (syntax-rules (use define-sprite)
    [(_ land-id lands (use land) decl ...)
     (begin
       (use land)
       (top land-id (land . lands) decl ...))]
    [(_ land-id (land ...) (define-sprite id . rest) ...)
     (begin
       (define land-id (new land%
                            [get-lands (lambda () (list land ...))]))
       (provide land-id)
       (define-sprite land-id id . rest) ...
       (provide id ...)
       (module+ main (run+ land-id)))]))

;; ----------------------------------------

(define-syntax (use stx)
  (syntax-case stx ()
    [(_ land-id)
     (with-syntax ([path-spec (datum->syntax #'here
                                             (format "~a.rkt" (syntax-e #'land-id))
                                             #'land-id
                                             #'land-id)])
       #`(begin
           (define-runtime-module-path-index other-land-path path-spec)
           (define lazy-land-id (delay (dynamic-require other-land-path 'land-id)))
           (define-syntax (land-id stx) #'(force lazy-land-id))
           (define-syntax (module stx)
             (import-definitions stx (quote-syntax land-id) (quote-syntax other-land-path)))
           (include-at/relative-to module land-id path-spec)))]))

(define-for-syntax (import-definitions stx context-id path-id)
  (syntax-case stx ()
    [(_ name lang (mb elem ...))
     #`(begin #,@(for/list ([elem (syntax->list #'(elem ...))])
                   (syntax-case elem (use define-sprite)
                     [(use . _) #'(begin)]
                     [(define-sprite id . _)
                      (with-syntax ([id (syntax-local-introduce
                                         (datum->syntax context-id
                                                        (syntax-e #'id)
                                                        context-id
                                                        context-id))]
                                    [(lazy-id) (generate-temporaries (list #'id))]
                                    [path-id path-id])
                        #'(begin
                            (define-syntax (id stx) #'(force lazy-id))
                            (define lazy-id (delay (dynamic-require path-id 'id)))))])))]))

;; ----------------------------------------

(define-syntax (define-sprite stx)
  (syntax-parse stx
    [(_ land-id name:id (~or (~once (~seq #:image image-expr:expr))
                           (~optional (~seq #:x x-expr:expr)
                                      #:defaults ([x-expr #'0]))
                           (~optional (~seq #:y y-expr:expr)
                                      #:defaults ([y-expr #'0]))
                           (~optional (~seq #:size size-expr:expr)
                                      #:defaults ([size-expr #'1]))
                           (~optional (~seq #:direction dir-expr:expr)
                                      #:defaults ([dir-expr #'90]))
                           (~seq #:key key-name:id key-expr:expr ...)
                           (~seq #:message msg:str msg-expr:expr ...)
                           (~seq #:task task-expr:expr ...)
                           (~seq #:variable variable-name:id variable-expr:expr))
        ...)
     #'(define-sprite/method name
         [land land-id]
         [image image-expr]
         [x x-expr]
         [y y-expr]
         [size size-expr]
         [direction dir-expr]
         [key-callback
          (key-handler
           [key-name key-expr ...]
           ...)]
         [message-callback
          (lambda (msg-in)
            (cond
             [(equal? msg-in msg) msg-expr ...]
             ...))]
         [variable variable-name variable-expr] ...
         [task task-expr ...] ...)]))

;; ----------------------------------------

(define-syntax-parameter self #f)

(define-syntax-rule (define-sprite/method name clause ...)
  (splicing-syntax-parameterize ([self (syntax-rules ()
                                         [(_) name])])
    (define-sprite/orig name clause ...)))


(define-syntax-rule (provide-sprite-method id ...)
  (begin
    (provide id ...)
    (define-sprite-method id ...)))

(define-syntax-rule (define-sprite-method id ...)
  (begin
    (...
     (define-syntax-rule (id arg ...)
       (send (self) id arg ...)))
    ...))


(define-syntax-rule (provide-land-method id ...)
  (begin
    (provide id ...)
    (define-land-method id ...)))

(define-syntax-rule (define-land-method id ...)
  (begin
    (...
     (define-syntax-rule (id land arg ...)
       (send land id arg ...)))
    ...))

(provide-methods)

;; ----------------------------------------

(define-syntax-rule (provide-keyword id ...)
  (begin
    (define-syntax id #f) ...
    (provide id ...)))

(define-syntax do-keyword #f)

(provide-keywords)

;; ----------------------------------------

(define ready-sema (make-semaphore))
(define ready (semaphore-peek-evt ready-sema))

(define (run+ land)
  (begin0
   (run land)
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
