#lang slideshow
(require slideshow/code
         slideshow/step)

(provide implicit-slides)

(define expands (t "⇒"))

(define (implicit-slides)
  (with-steps~
   (->app ->datum ->quote ->module-begin)
   (slide
    #:title "Implicit Keywords"
    (table
     3
     (list (code (+ 1 2))
           expands
           (code (#%app + 1 2))
           (blank)
           ((vafter ->datum) expands)
           ((vafter ->datum)
            (code (#%app + 
                         (#%datum 1) 
                         (#%datum 2))))
           
           (blank)
           ((vafter ->quote) expands)
           ((vafter ->quote) (code (#%app + (quote 1) (quote 2))))
           
           (tt " ") (blank) (blank)
           (tt " ") (blank) (blank)
           
           ((vafter ->module-begin)
            (code (module m scheme
                    (define x 3)
                    (+ x 1))))
           ((vafter ->module-begin)
            expands)
           ((vafter ->module-begin)
            (code (module m scheme
                    (#%module-begin
                     (define x 3)
                     (+ x 1))))))
     (cons rtl-superimpose ltl-superimpose)
     (cons rtl-superimpose ltl-superimpose)
     (* 1 gap-size) (* 2 (current-code-line-sep)))))
  (void))

(module+ main
  (implicit-slides))
