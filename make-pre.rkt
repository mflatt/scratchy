#lang racket

(define dest "/tmp/pre-scratchy")

(define (adjust!)
  (rm "make-pre.rkt")
  (rm "README.txt")
  (rm "info.rkt")
  (rm "scratchy-tutorial.scrbl")
  (rm "fish11.rkt")
  (rm "noisy.rkt")
  (rm "fish12.rkt")
  (rm "black.rkt")
  (rm "fish.rkt")
  (rm "just-fish.rkt")
  (rm "splash.rkt")

  (delete-definition "task.rkt" 'forever '....)
  (delete-definition "task.rkt" 'while)
  (delete-definition "task.rkt" 'task)
  
  (delete-definition "sync-task.rkt" 'sync-task '....)
  (delete-definition "sync-task.rkt" 'ready-sema)
  (delete-definition "sync-task.rkt" 'ready)
  (delete-definition "sync-task.rkt" 'run+)
  
  (delete-definition "sprite.rkt" 'define-sprite '....)
  (delete-all-requires "sprite.rkt")

  (delete-definition "key.rkt" 'key-handler)
  (delete-line "key.rkt" #rx"Ok, but")
  (delete-line "key.rkt" #rx"Better")
  (delete-line "key.rkt" #rx"#;")
  (delete-all-requires "key.rkt")

  (delete-all-requires "method.rkt" '(require "sprite.rkt"))
  (delete-definition "method.rkt" 'self)
  (delete-definition "method.rkt" 'define-method)
  (delete-definition "method.rkt" 'define-sprite)
  (delete-application "method.rkt" 'define-method)

  (delete-application "almost-scratchy.rkt" 'except-out '#(.... #%module-begin ....))
  (delete-application "almost-scratchy.rkt" 'rename-out)
  (delete-definition "almost-scratchy.rkt" 'module-begin '....)

  (delete-lines-through "exact.rkt" #rx"The primitive way")
  (delete-line "exact.rkt" #rx"[|]#")
  (delete-definition "exact.rkt" 'read-syntax* '(define (read-syntax* src in) ....))

  (delete-identifier "s-exp.rkt" 'scratchy-tutorial/scratchy '....)
  
  (void))

(when (directory-exists? dest)
  (delete-directory/files dest))

(copy-directory/files "." dest)

(define (rm f)
  (delete-file (build-path dest f)))

(define (delete-line file-name rx)
  (define file (build-path dest file-name))
  (define lines (for/list ([l (file->lines file)]
                           #:unless (regexp-match? rx l))
                  l))
  (with-output-to-file file
    #:exists 'truncate
    (lambda ()
      (for ([l (in-list lines)])
        (displayln l)))))

(define (delete-lines-through file-name rx)
  (define file (build-path dest file-name))
  (define lines
    (let loop ([ls (file->lines file)])
      (if (regexp-match? rx (car ls))
          (cdr ls)
          (loop (cdr ls)))))
  (with-output-to-file file
    #:exists 'truncate
    (lambda ()
      (for ([l (in-list lines)])
        (displayln l)))))

(define (delete-something file-name replacement match? not-found)
  (define file (build-path dest file-name))
  (define stx (parameterize ([read-accept-reader #t])
                (with-input-from-file file read-syntax)))
  (define target-stx
    (let loop ([stx stx])
      (or (match? stx)
          (syntax-case stx ()
            [(a ...)
             (ormap loop (syntax->list stx))]
            [_ #f]))))
  (unless target-stx
    (not-found file))
  (define start (sub1 (syntax-position target-stx)))
  (define span (syntax-span target-stx))
  (define s (file->string file))
  (define new-s (string-append (substring s 0 start) 
                               (if replacement
                                   (if (vector? replacement)
                                       (apply ~s (vector->list replacement))
                                       (~s replacement))
                                   "")
                               (substring s (+ start span) (string-length s))))
  (with-output-to-file file
    #:exists 'truncate
    (lambda () (write-string (regexp-replace* "\n\n+(\n|$)" new-s "\n\\1"))))
  (void))

(define (delete-definition file-name what [replacement #f])
  (delete-something
   file-name
   replacement
   (lambda (stx)
     (syntax-case stx ()
       [(dv id . _)
        (and (memq (syntax-e #'dv) '(define define-syntax define-syntax-parameter))
             (eq? (syntax-e #'id) what))
        stx]
       [(dv (id . _) . _)
        (and (memq (syntax-e #'dv) '(define define-syntax define-syntax-rule))
             (eq? (syntax-e #'id) what))
        stx]
       [_ #f]))
   (lambda (file)
     (error 'delete-definition 
            (~a "not found\n"
                "  name: ~a\n"
                "  file: ~a")
            what
            file))))

(define (delete-application file-name what [replacement #f])
  (delete-something
   file-name
   replacement
   (lambda (stx)
     (syntax-case stx ()
       [(id . _)
        (eq? (syntax-e #'id) what)
        stx]
       [_ #f]))
   (lambda (file)
     (error 'delete-application
            (~a "not found\n"
                "  form name: ~a\n"
                "  file: ~a")
            what
            file))))

(define (delete-identifier file-name what [replacement #f])
  (delete-something
   file-name
   replacement
   (lambda (stx)
     (and (eq? (syntax-e stx) what)
          stx))
   (lambda (file)
     (error 'delete-identifier
            (~a "not found\n"
                "  id: ~a\n"
                "  file: ~a")
            what
            file))))

(define (delete-all-requires file-name [replacement #f])
  (delete-something
   file-name
   replacement
   (lambda (stx)
     (syntax-case stx ()
       [(req . _)
        (eq? (syntax-e #'req) 'require)
        stx]
       [_ #f]))
   (lambda (file)
     (error 'delete-all-requires
            (~a "`require' not found\n"
                "  file: ~a")
            file))))

(adjust!)
