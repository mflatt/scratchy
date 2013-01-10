#lang slideshow
(require slideshow/code
         slideshow/balloon
         racket/draw
         (only-in texpict/mrpict record))

(provide parsing-slides
         parsing-examples-slides
         parsing-rationale-slides)

;; ----------------------------------------

(define (ptt s) (colorize (tt s) (current-base-color)))
(define (pstr s) (colorize (tt s) (current-literal-color)))
(define (rec-tt s) (inset (scale (tt s) 0.8) 5))

(define fade (lambda (p) (cellophane p 0.25)))

(define (rec-sub r) (inset r 5))

(define (round-wrap #:color [col "blue"] p)
  (let ([p (inset
            p
            gap-size)])
    (cc-superimpose
     (cellophane (colorize (filled-rounded-rectangle (pict-width p) (pict-height p) gap-size) col)
                 0.25)
     p)))

(define (parse-stage p) (round-wrap p #:color "lightblue"))
(define s-expression-stage
  (parse-stage
   (vc-append
    (current-line-sep)
    (code (...
           (....)
           ...))
    (t "S-expression"))))
(define ast-stage
  (parse-stage
   (vc-append
    (current-line-sep)
    (let ([p (record (rec-tt "function")
                     (rec-tt "args")
                     (rec-tt "body"))])
      (cc-superimpose
       (colorize (filled-rectangle (pict-width p) (pict-height p)) "white")
       p))
    (t "AST"))))

(define (as-file* p)
  (as-file #f (inset p 5 10)))
    
(define (as-file s content)
  (let ([w (max (* 3 gap-size)
                (+ 6 (pict-width content)))]
        [h (max (* 4 gap-size)
                (+ gap-size (pict-height content)))])
    (let ([icon (file-icon w h "beige")])
      (let ([file (cc-superimpose
                   icon
                   content)])
        (if (not s)
            file
            (inset
             (vc-append
              (current-line-sep)
              file
              s)
             0 0 (/ (min 0 (- (pict-width icon) (pict-width s))) 2) 0))))))

(define trans-arrow (colorize (arrow (* 2 gap-size) 0) "forestgreen"))   

(define (pipeline left-adj left-arrow-decorate mid-adj right-arrow-decorate right-adj)
  (hc-append
   (* 2 gap-size)
   (left-adj (parse-stage (as-file (t "Source") (blank))))
   (left-adj (left-arrow-decorate trans-arrow))
   (mid-adj s-expression-stage)
   (right-adj (right-arrow-decorate trans-arrow))
   (right-adj ast-stage)))

;; ----------------------------------------

(define (pin-arrows-line* sz p src src-find dest dest-find #:line-width lw #:color col)
  (let-values ([(sx sy) (src-find p src)]
               [(dx dy) (dest-find p dest)]
               [(APART) (* gap-size -4)])
    (pin-over 
     p
     0 0
     (colorize
      (linewidth
       lw
       (let* ([p
               (dc (lambda (dc x y)
                     (let ([b (send dc get-brush)])
                       (send dc set-brush "white" 'transparent)
                       (let ([draw
                              (lambda (del)
                                (let ([path (new dc-path%)]
                                      [dh (/ (- dy sy) 4)])
                                  (send path move-to sx sy)
                                  (send path curve-to 
                                        (del dx (/ APART 2)) (+ sy dh)
                                        (del dx (/ APART 2)) (- dy dh)
                                        dx dy)
                                  (send dc draw-path path (- x (del (/ gap-size 2))) y)))])
                         (draw -)
                         (draw +))
                       (send dc set-brush b)))
                   0 0)]
              [p (pin-arrow-line sz
                                 p
                                 p (lambda x (values (- dx (/ gap-size 2) 2) (+ dy 0)))
                                 p (lambda x (values (- dx (/ gap-size 2) -2) (- dy 4))))]
              [p (pin-arrow-line sz
                                 p
                                 p (lambda x (values (+ sx (/ gap-size 2) 2) (- sy 0)))
                                 p (lambda x (values (+ sx (/ gap-size 2) -2) (+ sy 4))))])
         p))              
      col))))
    
(define ((decorate-arrow sep show-top top show-bottom bottom) trans-arrow)
  (refocus
   (let* ([arr (inset trans-arrow 0 (/ gap-size 2))]
          [p (vc-append
              (* sep gap-size)
              (show-top top)
              arr
              (show-bottom bottom))])
     (let ([pin-one
            (lambda (show p pin-arrow-line end end-find arr-find)
              (cc-superimpose
               p
               (show
                (pin-arrow-line (/ gap-size 2)
                                (ghost p )
                                end end-find
                                arr arr-find
                                #:line-width 2
                                #:color "red"))))])
       (let* ([p (pin-one show-top p pin-arrow-line
                          top cb-find ct-find)]
              [p (pin-one show-bottom p pin-arrows-line*
                          bottom ct-find cb-find)])
         p)))
   trans-arrow))
    
;; ----------------------------------------

(define parsing-title "Parsing")
    
(define (xform lang reader mod req left-adj right-adj)
  (pipeline
   left-adj
   (decorate-arrow 6 lang (code #,(tt "#lang") _path) reader (code #,(tt "#reader") _path))
   values
   (decorate-arrow 4 mod (code (module _name _path ...)) req (code (require _path)))
   right-adj))

(define (xform-slide title lang reader mod req)
  (slide
   #:title title
   (xform lang reader mod req values values)))
   
(define (parsing-slides #:title [title parsing-title]) 
  (xform-slide title ghost ghost ghost ghost)
  (xform-slide title values ghost ghost ghost)
  (xform-slide title values ghost values ghost)
  (xform-slide title values ghost values values)
  (xform-slide title values values values values))
    
;; ----------------------------------------
    
(define scheme-orig
  (as-file*
   (code #,(tt "#lang") scheme
         (define (hi)
           "Hello"))))
(define scheme-mod
  (code (module m scheme
          (define (hi)
            "Hello"))))
(define scheme-core
  (record
   (rec-sub
    (record
     (rec-tt "define")
     (ht-append
      gap-size
      (rec-tt "hi")
      (rec-sub
       (record
        (rec-tt "function")
        (rec-tt "()")
        (rec-tt "\"Hello\""))))))))

(define scribble-orig
  (as-file*
   (vl-append
    (current-code-line-sep)
    (code #,(tt "#lang") scribble/doc)
    (htl-append (tt "@") (code (require 
                                scribble/manual)))
    (hbl-append (tt "@")
                (code bold)
                (ptt "{")
                (colorize (tt "Hi") (current-literal-color))
                (ptt "}")))))
(define scribble-mod
  (code
   (module m doclang
     (require 
      scribble/manual)
     (bold "Hi"))))
(define scribble-core
  (record
   (rec-sub
    (ht-append
     gap-size
     (vl-append
      (/ gap-size 2)
      (record
       (rec-tt "import")
       (rec-tt "scribble/doc")
       (rec-tt "scribble/manual"))
      (record
       (rec-tt "export")
       (rec-tt "doc")))
     (record
      (rec-tt "define")
      (rec-tt "doc")
      (rec-sub
       (record
        (rec-tt "apply")
        (rec-tt "bold")
        (rec-tt "(\"Hi\")"))))))))

(define honu-orig
  (as-file*
   (vl-append
    (current-code-line-sep)
    (code #,(tt "#lang") honu)
    (hbl-append (code 1) (code +) (code 2) (ptt ";")))))
(define honu-mod
  (code (module m honu
          1 + 2 #,(colorize (tt "|;|") (current-id-color)))))
(define honu-core
  (record
   (rec-sub
    (ht-append
     gap-size
     (record
      (rec-tt "import")
      (rec-tt "honu-procs"))
     (record
      (rec-tt "apply")
      (rec-tt "print")
      (rec-sub
       (record
        (rec-tt "apply")
        (rec-tt "+")
        (rec-tt "(1 2)"))))))))

;; ----------------------------------------

(define-syntax-rule (id s)
  (colorize (tt (symbol->string 's)) (current-id-color)))

(define (parsing-examples-slides #:title [title parsing-title])
  (slide
   #:title title
   (scale
    (table
     5
     (let ([trans-arrow (inset (scale trans-arrow 0.5)
                               0 gap-size 0 0)])
       (list scheme-orig trans-arrow scheme-mod trans-arrow scheme-core
             scribble-orig trans-arrow scribble-mod trans-arrow scribble-core
             honu-orig trans-arrow honu-mod trans-arrow honu-core))
     lc-superimpose lc-superimpose
     (* 1.5 gap-size) (* 3 gap-size))
    0.75)))

(define (parsing-rationale-slides #:title [title parsing-title])
  (slide
   #:title title
   'alts
   (list 
    (list (xform ghost ghost ghost ghost values values))
    (list
     (xform ghost ghost ghost ghost values fade)
     (blank)
     (blank)
     (para #:align 'center (bit "Read") "layer provides absolute control")
     (blank)
     (scale
      (hc-append (* 3 gap-size)
                 (code (+ 1 2)) 
                 (hbl-append (tt "@") 
                             (code bold) 
                             (ptt "{") 
                             (colorize (tt "Hi") (current-literal-color))
                             (ptt "}") )
                 (hbl-append (code 1) (code +) (code 2)))
      0.75))
    (list
     (xform ghost ghost ghost ghost fade values)
     (blank)
     'alts
     (list
      (list
       (blank)
       (para #:align 'center (bit "Expand") "layer can delay ``inside'' until after ``outside''")
       (blank)
       (ht-append
        (* 4 gap-size)
        (scale
         (code
          (define-place start .... 
            ([north house-front]
             [south desert]))
          code:blank
          (define-place house-front .... 
            ([in room]
             [south start])))
         0.75)
        (scale
         (code int #,(hbl-append (id is_odd) (code (int x))) #,(ptt "{")
               code:blank ... #,(hbl-append (id is_even) (code (#,(hbl-append (code x) (code -) (code 1)))) (ptt ";"))
               #,(ptt "}")
               code:blank
               int #,(hbl-append (id is_even) (code (int x))) #,(ptt "{")
               code:blank ... #,(hbl-append (id is_odd) (code (#,(hbl-append (code x) (code -) (code 1)))) (ptt ";"))
               #,(ptt "}"))
         0.75)))
      (list
       (blank)
       (para "Lots of support for transformations at this layer:")
       (item "Pattern-matching macros")
       (item "Syntax objects that track binding and source location")
       (item "Compile-time introspection")))))))

;; ----------------------------------------

(module+ main
  (parsing-slides)
  (parsing-examples-slides #:title "Examples")
  (parsing-rationale-slides #:title "Rationale"))
