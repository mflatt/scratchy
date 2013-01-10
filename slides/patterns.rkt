#lang slideshow
(require slideshow/code
         slideshow/step
         racket/class
         racket/draw
         (for-syntax syntax/stx))

(provide simple-pattern-slides
         pattern-slides
         define-syntax-slides
         syntax-parse-slides)

(current-keyword-list (cons "syntax-parse" (current-keyword-list)))

;; Unusual indentation needed:
;;   let^1
;;   let-one
;; Watch out for (#,...) in two cases

(define GreenColor "green")
(define DarkGreenColor "forest green")
(define BlueColor "blue")
(define RedColor "red")
(define BeigeColor (make-object color% 255 255 200))

(define dt bit)

(define expands (colorize (t "⇒") "blue"))

(define longish (- client-w (* 2 gap-size)))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define doc-w (* 3/4 client-w))

(define (color-box p color)
  (cc-superimpose
   (colorize
    (dc (lambda (dc x y)
          (send dc draw-rectangle x y (pict-width p) (pict-height p)))
        (pict-width p) (pict-height p) (pict-ascent p) (pict-descent p))
    color)
   p))
(define light-shade (make-object color% 195 195 255))
(define (darker c)
  (scale-color 0.95 c))

(define (itt s)
  (text s `(italic . ,(current-code-font)) (current-font-size)))

(define pattern (color-box (itt "pattern") light-shade))
(define template (color-box (itt "template") (darker light-shade)))

(define cloud-shade "lightgray") ; (make-object color% 195 255 195))
(define (encloud p shade)
  (let ([n (cc-superimpose
            (cloud (* 9/8 (pict-width p))
                   (* 3/2 (pict-height p))
                   shade)
            p)])
    (lift-above-baseline (drop-below-ascent n (- (pict-ascent p))) (- (pict-descent p)))))

(define-for-syntax (annotate-pat-tmpls loop light-shade cloud-shade pat--tmpls)
  (map (lambda (pt)
         (if (stx-pair? pt)
             (let ([tmpl (loop (stx-car (stx-cdr pt)) 
                               #`(darker (darker #,light-shade))
                               cloud-shade)]
                   [pat (stx-car pt)])
               (with-syntax ([us #'unsyntax])
                 (datum->syntax
                  pt
                  (list
                   #`(us (datum->syntax
                          #f
                          (color-box (code #,pat) #,light-shade)
                          (quote-syntax #,pat)))
                   #`(us (datum->syntax
                          #f
                          (color-box (code #,tmpl) (darker #,light-shade))
                          (quote-syntax #,(stx-car (stx-cdr pt))))))
                  pt
                  pt)))
             (loop pt light-shade cloud-shade)))
       pat--tmpls))

;; The scode macro is like code, except that it
;;   - Shades behind patterns and templates (by recognizing keywords)
;;   - Puts clouds around expr for (code:encloud expr)
;; Much of the complexity has to do with preserving source-location
;;  info, which is crucial to proper typesetting of the code
;; >>> Some cut and paste here should be cleaned up! <<<
(define-syntax (scode stx)
  (syntax-case stx ()
    [(_ stx ...)
     #`(code
        #,@(map
            (lambda (stx)
              (let loop ([stx stx][light-shade #'light-shade][cloud-shade #'cloud-shade])
                (syntax-case stx (..... code:encloud)
                  [(ds n b)
                   (and (identifier? #'ds)
                        (free-identifier=? #'ds #'define-syntax))
                   (datum->syntax
                    stx
                    (list
                     #'ds
                     (loop #'n light-shade cloud-shade)
                     (loop #'b light-shade cloud-shade))
                    stx)]
                  [(sr pat--tmpl ...)
                   (and (identifier? #'sr)
                        (or (free-identifier=? #'sr #'identifier-syntax)))
                   (let ([pat--tmpls (syntax->list #'(pat--tmpl ...))])
                     (datum->syntax
                      stx
                      (list*
                       #'sr
                       (annotate-pat-tmpls loop light-shade cloud-shade pat--tmpls))
                      stx))]
                  [(sr kws pat--tmpl ...)
                   (and (identifier? #'sr)
                        (or (free-identifier=? #'sr #'syntax-rules)
                            (free-identifier=? #'sr #'syntax-id-rules)))
                   (let ([pat--tmpls (syntax->list #'(pat--tmpl ...))])
                     (datum->syntax
                      stx
                      (list*
                       #'sr
                       #'kws
                       (annotate-pat-tmpls loop light-shade cloud-shade pat--tmpls))
                      stx))]
                  [(sc expr kws pat--expr ...)
                   (and (identifier? #'sc)
                        (free-identifier=? #'sc #'syntax-case))
                   (let ([pat--exprs (syntax->list #'(pat--expr ...))])
                     (datum->syntax
                      stx
                      (list*
                       #'sc
                       (loop #'expr light-shade cloud-shade)
                       #'kws
                       (map (lambda (pe)
                              (if (stx-pair? pe)
                                  (let ([expr (stx-car (stx-cdr pe))]
                                        [pat (stx-car pe)])
                                    (with-syntax ([us #'unsyntax])
                                      (datum->syntax
                                       pe
                                       (list
                                        #`(us (datum->syntax
                                               #f
                                               (color-box (code #,pat) #,light-shade)
                                               (quote-syntax #,pat)))
                                        (loop expr light-shade cloud-shade))
                                       pe
                                       pe)))
                                  (loop pe light-shade cloud-shade)))
                            pat--exprs))
                      stx))]
                  [(sc expr pat--expr ...)
                   (and (identifier? #'sc)
                        (free-identifier=? #'sc #'syntax-parse))
                   (let ([pat--exprs (syntax->list #'(pat--expr ...))])
                     (datum->syntax
                      stx
                      (list*
                       #'sc
                       (loop #'expr light-shade cloud-shade)
                       (map (lambda (pe)
                              (if (stx-pair? pe)
                                  (let ([expr (stx-car (stx-cdr pe))]
                                        [pat (stx-car pe)])
                                    (with-syntax ([us #'unsyntax])
                                      (datum->syntax
                                       pe
                                       (list
                                        #`(us (datum->syntax
                                               #f
                                               (color-box (code #,pat) #,light-shade)
                                               (quote-syntax #,pat)))
                                        (loop expr light-shade cloud-shade))
                                       pe
                                       pe)))
                                  (loop pe light-shade cloud-shade)))
                            pat--exprs))
                      stx))]
                  [(ws pat--expr body)
                   (and (identifier? #'ws)
                        (free-identifier=? #'ws #'with-syntax))
                   (let ([pat--exprs (syntax->list #'pat--expr)])
                     (datum->syntax
                      stx
                      (list
                       #'ws
                       (datum->syntax
                        #'pat--expr
                        (map (lambda (pe)
                               (if (stx-pair? pe)
                                   (let ([expr (stx-car (stx-cdr pe))]
                                         [pat (stx-car pe)])
                                     (with-syntax ([us #'unsyntax])
                                       (datum->syntax
                                        pe
                                        (list
                                         #`(us (datum->syntax-object
                                                #f
                                                (color-box (code #,pat) #,light-shade)
                                                (quote-syntax #,pat)))
                                         (loop expr light-shade cloud-shade))
                                        pe)))
                                   (loop pe light-shade cloud-shade)))
                             pat--exprs)
                        #'pat--expr)
                       (loop #'body light-shade cloud-shade))
                      stx))]
                  [(sx tmplt)
                   (and (identifier? #'sx)
                        (free-identifier=? #'sx #'syntax))
                   (let ([tmpl (loop #'tmplt
                                     #`(darker (darker #,light-shade))
                                     cloud-shade)])
                     (datum->syntax
                      stx
                      (list
                       #'sx
                       (with-syntax ([us #'unsyntax])
                         #`(us (datum->syntax
                                #f
                                (color-box (code #,tmpl) (darker #,light-shade))
                                (quote-syntax tmplt)))))
                      stx))]
                  [(code:encloud x)
                   (with-syntax ([us #'unsyntax])
                     #`(us (datum->syntax #f (encloud (code #,(loop #'x light-shade
                                                                    #`(darker #,cloud-shade)))
                                                      #,cloud-shade)
                                          (quote-syntax #,stx))))]
                  [(a . b)
                   (datum->syntax
                    stx
                    (cons (loop #'a light-shade cloud-shade) (loop #'b light-shade cloud-shade))
                    stx
                    stx)]
                  [..... 
                   (with-syntax ([us #'unsyntax])
                     #`(us (datum->syntax #f .....-val (quote-syntax #,stx))))]
                  [x #'x])))
            (syntax->list #'(stx ...))))]))

(define (introduced id)
  (colorize (parameterize ([code-colorize-enabled #f])
              (typeset-code id))
            RedColor))

(define (expands-table . l)
  (table 3
         l
         ltl-superimpose ltl-superimpose
         gap-size (current-line-sep)))

(define swap-defn
  (scode
   (define-syntax-rule (swap a b)
     (let ([tmp b])
       (set! b a)
       (set! a tmp)))))

(define .....-val (let ([p (code .....)])
                    (refocus (cc-superimpose 
                              (ghost p)
                              (scale (cloud (pict-width p) (pict-height p)) 0.95))
                             p))) 

;; ----------------------------------------

(define (simple-pattern-slides)
  (void
   (with-steps
    (dsr pat-tmpl pattern template)
    (slide
     #:title "Simple Pattern-Based Macros"
     (lt-superimpose
      ((vbetween dsr dsr)
       (scode
        (define-syntax-rule .....
          .....)))
      ((vbetween pat-tmpl pat-tmpl)
       (scode
        (define-syntax-rule #,pattern
          #,template)))
      ((vbetween pattern pattern)
       (scode
        (define-syntax-rule (swap a b)
          .....)))
      ((vafter template)
       swap-defn))
     (blank)
     (ct-superimpose
      ((vbetween dsr dsr)
       (item (code define-syntax-rule) "indicates a simple-pattern macro definition"))
      ((vbetween pat-tmpl pat-tmpl)
       (vc-append
        gap-size
        (item "A" pattern "to match")
        (item "Produce result from" template)))
      ((vbetween pattern pattern)
       (vc-append
        gap-size
        (item "Pattern for this macro:" (code (swap a b)))
        (item "Each identifier matches anything in use")
        (expands-table
         (code (swap x y))
         expands
         (para #:fill? #f (code a) (t "is") (code x))
         (blank)(blank)
         (para #:fill? #f (code b) (t "is") (code y))
         (tt " ")(blank)(blank)
         (code (swap 9 (+ 1 7)))
         expands
         (para #:fill? #f (code a) (t "is") (code 9))
         (blank)(blank)
         (para #:fill? #f (code b) (t "is") (code (+ 1 7))))))
      ((vbetween template template)
       (vc-append
        gap-size
        (para #:width longish
              "Bindings substituted into template to generate the result")
        (expands-table
         (code (swap x y))
         expands
         (code (let ([tmp y])
                 (set! y x)
                 (set! x tmp)))
         (tt " ")(blank)(blank)
         (code (swap 9 (+ 1 7)))
         expands
         (code (let ([tmp (+ 1 7)])
                 (set! (+ 1 7) 9)
                 (set! 9 tmp)))))))))))

;; ----------------------------------------

(define (pattern-slides)
  (void
   (with-steps
    (shift ds rs pat-tmpl pattern template)
    (slide
     #:title "Pattern-Based Macros"
     (lt-superimpose
      ((vbetween shift ds)
       (scode
        (define-syntax shift
          .....)))
      ((vbetween rs rs)
       (scode
        (define-syntax shift
          (syntax-rules (back)
            .....))))
      ((vbetween pat-tmpl pat-tmpl)
       (code
        (define-syntax shift
          (syntax-rules (back)
            [#,pattern #,template]
            ...
            [#,pattern #,template]))))
      ((vbetween pattern pattern)
       (scode
        (define-syntax shift
          (syntax-rules (back)
            [(shift a b c) .....]
            [(shift back a b c) .....]))))
      ((vafter template)
       (scode
        (define-syntax shift
          (syntax-rules (back)
            [(shift a b c) (begin
                             (swap a b)
                             (swap b c))]
            [(shift back a b c) (begin
                                  (swap c b)
                                  (swap b a))])))))
     (blank)
     (ct-superimpose
      ((vbetween shift shift)
       (htl-append
        (* 3 gap-size)
        (code
         (let ([x 0]
               [y 1]
               [z 2])
           (shift x y z)))
        (code
         (let ([x 0]
               [y 1]
               [z 2])
           (shift back x y z)))))
      ((vbetween ds ds)
       (item (code define-syntax) "indicates a macro definition"))
      ((vbetween rs rs)
       (vc-append
        gap-size
        (item (code syntax-rules) "means a pattern-matching macro")
        (item (code (back)) "means that" (code back) "is literal in patterns")))
      ((vbetween pat-tmpl pat-tmpl)
       (vc-append
        gap-size
        (item "Any number of" (hbl-append pattern (t "s")) "to match")
        (item "Produce result from" template "of first match")))
      ((vbetween pattern pattern)
       (vc-append
        gap-size
        (para "Two patterns for this macro")
        (item (code (shift x y z)) "matches first pattern")
        (item (code (shift back x y z)) "matches second pattern")))
      ((vbetween template template)
        (expands-table
         (code (shift x y z))
         expands
         (code (begin
                 (swap x y)
                 (swap y z)))
         (tt " ")(blank)(blank)
         (code (shift back x y z))
         expands
         (code (begin
                 (swap z y)
                 (swap y x))))))))))

;; ----------------------------------------

(define (define-syntax-slides)
  (slide
   #:title
   "Transformer Definitions"
   (para #:width longish
         "In general," (code define-syntax) "binds a transformer procedure:")
   (blank)
   (vl-append
    gap-size
    (scode (define-syntax swap
             (syntax-rules .....)))
    expands
    (scode (define-syntax swap
             (lambda (stx)
               (code:encloud #,(vc-append
                                (para #:fill? #f 
                                      "use syntax-object primitives to")
                                (para #:fill? #f
                                      (t "match") (code stx)
                                      (t "and generate result"))))))))))

;; ----------------------------------------

(define (syntax-parse-slides)
  
  (define syntax-parse-title "Matching Syntax and Having It, Too")
  
  (slide
   #:title syntax-parse-title
   (para #:width longish
         (code syntax-parse) "and" (colorize (tt "#'") keyword-color)
         "combine patterns and computation")
   (vl-append
    gap-size
    (scode (syntax-parse _stx-expr
             [_pattern _result-expr]
             ...
             [_pattern _result-expr]))
    (blank)
    (scode #'template)))
  
  (slide
   #:title syntax-parse-title
   (vl-append
    gap-size
    swap-defn
    expands
    (scode (define-syntax swap
             (lambda (stx)
               (syntax-parse stx
                 [(swap_1 a b) #'(let ([tmp b])
                                   (set! b a)
                                   (set! a tmp))]))))))
  
  (slide
   #:title syntax-parse-title
   (para #:width longish "Check for identifiers before expanding:")
   (scode (define-syntax swap
            (lambda (stx)
              (syntax-parse stx
                [(swap a b) 
                 (if (and (identifier? #'a) 
                          (identifier? #'b))
                     #'(let ([tmp b])
                         (set! b a)
                         (set! a tmp))
                     (raise-syntax-error
                      'swap "needs identifiers" 
                      stx))]))))))

;; ----------------------------------------

(module+ main
  (simple-pattern-slides)
  (pattern-slides)
  (define-syntax-slides)
  (syntax-parse-slides))
