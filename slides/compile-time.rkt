#lang slideshow
(require slideshow/code
         "hilite.rkt")

(provide compile-time-slides
         tone-slides)

;; ----------------------------------------

(define-code CODE typeset-code U)

;; ----------------------------------------

(define (note-slide #:bfs [bfs values]
                    #:bfs2 [bfs2 values]
                    #:ds0 [ds0 values]
                    #:tone-decl [tone-decl values]
                    #:tone [tone values]
                    #:all-A [all-A values]
                    #:A [A all-A]
                    #:check [check values]
                    #:check-proc [check-proc values]
                    #:check-use [check-use values]
                    #:slv [slv values])
  (slide
   #:title "Compile-Time Introspection"
   (scale
    (code
     #,(let-syntax ([begin-for-syntax (make-code-transformer #'(bfs (code begin-for-syntax)))])
         (code
          (begin-for-syntax
           #,(tone-decl (code (struct #,(tone (code tone)) (freq))))
           (struct duration (len)))))
     code:blank
     (#,(ds0 (code define-syntax)) #,(A (code A)) (#,(tone (code tone)) #'440))
     (define-syntax ♩ (duration #'1/4))
     code:blank
     #,(let-syntax ([begin-for-syntax (make-code-transformer #'(bfs2 (code begin-for-syntax)))])
         (check
          (code
           (begin-for-syntax
            (define (#,(check-proc (code check)) id pred what stx)
              (unless (pred (syntax-local-value id (lambda () #f)))
                (raise-syntax-error #f (~a "not a " what) id stx)))))))
     code:blank
     #,(CODE
        (define-syntax (note stx)
          (syntax-parse stx
            [(_ (U (hbl-append (all-A (code tn)) (code :id))) dr:id)
             (U (check (code (#,(check-use (code check)) #'tn tone? "tone" stx))))
             (U (check (code (#,(check-use (code check)) #'dr duration? "duration" stx))))
             #`(list #,(tone-freq ((U (slv (code syntax-local-value))) #'(U (all-A (code tn)))))
                     #,(duration-len (syntax-local-value #'dr)))])))
     code:blank
     (note #,(all-A (code A)) ♩))
    3/4)))

(define (compile-time-slides)
  (note-slide)
  (note-slide #:bfs (hilite hilite-color "start compile-time code"
                            #:pin-find rt-find
                            #:spike-dx (- gap-size) #:spike-dy 0))
  (note-slide #:tone-decl (hilite note-color "a compile-time record declaration"
                                  #:pin-find rt-find))
  (note-slide #:tone (hilite note-color)
              #:ds0 (hilite hilite-color)
              #:A (hilite #f "bind" (code A) "to a compile-time record"))
  (note-slide #:check fade
              #:all-A (hilite note-color)
              #:slv (hilite hilite-color "compile-time lookup"))
  (note-slide #:check-proc (hilite note-color "compile-time helper function"
                                   #:pin-find rt-find
                                   #:spike-dx (- gap-size))
              #:check-use (hilite note-color)
              #:bfs2 (hilite hilite-color)))

;; ----------------------------------------

(define (tone-slide #:prop [prop values]
                    #:method [method values]
                    #:pat [pat values]
                    #:tmpl [tmpl values])
  (slide
   #:title "Structures as Procedures"
   (scale
    (let-syntax ([_:id (make-code-transformer #'(hbl-append (colorize (tt "_:id") 
                                                                      (current-id-color))))]
                 [lambda (make-code-transformer #'(method (code lambda)))])
      (code
       (begin-for-syntax
        (struct tone (freq)
          #:property 
          #,(prop (code prop:procedure))
          (lambda (self stx)
            (syntax-parse stx
              [#,(pat (code _:id)) #,(tmpl (code (tone-freq self)))]))))
       code:blank
       (define-syntax A (tone #'440))
       code:blank
       (note A ♩) (code:comment #,(hbl-append (t "⇒") (tt " ") (code (list 440 1/4))))
       A (code:comment #,(hbl-append (t "⇒") (tt " ") (code 440)))))
    0.75)))

(define (tone-slides)
  (tone-slide)
  (define prop (hilite hilite-color "adds an ``apply'' method to the structure"
                       #:spike-dx (- gap-size) #:spike-dy 0
                       #:pin-find rc-find))
  (tone-slide #:prop prop)
  (tone-slide #:prop prop #:method (hilite #f "method acts as a macro transformer"
                                           #:width (/ client-w 4)
                                           #:spike-dx (* 2 gap-size)
                                           #:spike 'e))
  (tone-slide #:pat (hilite patcol)
              #:tmpl (hilite tmplcol
                             "macro expansion is" (code freq) "field"
                             #:pin-find (lambda (a b)
                                          (define-values (x y) (ct-find a b))
                                          (values (+ x (* 2 gap-size)) y)))))

;; ----------------------------------------

(module+ main
  (compile-time-slides)
  (tone-slides))
