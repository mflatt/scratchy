#lang slideshow
(require slideshow/code
         "hilite.rkt")

(provide stxparam-slides)

(current-keyword-list (list* "syntax-parameterize" 
                             "define-syntax-parameter" 
                             (current-keyword-list)))

(define (unbound id)
  (hbl-append (t "⇒") 
              (tt " ")
              (colorize (it " error: unbound identifier ") "red")
              (tt id)))

(define (stxparam-slides)
  (slide
   #:title "Lexical Scope"
   (code
    (define (map f lst)
      (if (empty? lst)
          empty
          (cons (#,((hilite note-color) (code f)) (first lst))
                (map f (rest lst)))))
    code:blank
    (map (lambda (v) #,((hilite fail-color) (code lst)))
         '(1 2 3))
    (code:comment #,(unbound "lst"))))

  (define (dyn-scope-slide #:thunk [thunk values]
                           #:cop [cop values]
                           #:prm [prm values])
    (slide
     #:title "Parameters for Dynamic Scope"
     (let-syntax ([current-output-port (make-code-transformer #'(cop (code current-output-port)))]
                  [parameterize (make-code-transformer #'(prm (code parameterize)))])
       (code
        (define (with-output-to-file dest thunk)
          (define out (open-output-file dest))
          (parameterize ([current-output-port out])
            #,(thunk (code (thunk)))))
        code:blank
        (with-output-to-file "log"
          #,(thunk
             (code
              (lambda ()
                (fprintf (current-output-port) "done\n")))))))))

  (dyn-scope-slide #:thunk (hilite note-color))
  (dyn-scope-slide #:cop (hilite note-color))
  (dyn-scope-slide #:cop (hilite note-color) #:prm (hilite hilite-color))

  (slide
   #:title "Lexical Scope"
   (code
    (define-syntax-rule (aif tst thn els)
      (let ([#,((hilite note-color) (code it)) tst])
        (if it thn els)))
    code:blank
    (aif (get-current-fish)
         (send #,((hilite fail-color) (code it)) swim 100)
         'no-swimming)
    (code:comment #,(unbound "it"))))

  (define (stxparam-slide #:it [it values]
                          #:stxparam [stxparam values]
                          #:defstxparam [defstxparam values])
    (let-syntax ([syntax-parameterize (make-code-transformer #'(stxparam (code syntax-parameterize)))]
                 [define-syntax-parameter (make-code-transformer #'(defstxparam (code define-syntax-parameter)))])
      (slide
       #:title "Syntax-Parameter Scope"
       (code
        (define-syntax-parameter it
          (lambda (stx) 
            (raise-syntax-error #f 
                                "no enclosing binding" 
                                stx)))
        code:blank
        (define-syntax-rule (aif tst thn els)
          (let ([v tst])
            (syntax-parameterize ([#,(it (code it)) (lambda (stx) #'v)])
              (if v thn els))))
        code:blank
        (aif (get-current-fish)
             (send #,(it (code it)) swim 100)
             'no-swimming)
        (code:comment #,(hbl-append (t "⇒") 
                                    (tt " ")
                                    (t "make the current fish swim, if any")))))))

  (stxparam-slide #:it (hilite note-color))
  (stxparam-slide #:it (hilite note-color) #:stxparam (hilite hilite-color))
  (stxparam-slide #:it (hilite note-color) #:defstxparam (hilite hilite-color)))

;; ----------------------------------------

(module+ main
  (stxparam-slides))
