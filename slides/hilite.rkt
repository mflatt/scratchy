#lang slideshow
(require slideshow/balloon
         racket/draw
         racket/class)

(provide (all-defined-out))

(define hilite-color "yellow")
(define alt-hilite-color "greenyellow")
(define fail-color "pink")
(define runtime-color "lightblue")
(define comptime-color "thistle")
(define panel-color "lightblue")
(define note-color "wheat")

(define patcol (make-object color% 195 195 255))
(define (darker c)
  (scale-color 0.95 c))
(define tmplcol (darker patcol))

(define animation-progress (make-parameter 1))

(define (fade p) (cellophane p 0.4))

(define (hilite color 
                #:animate? [animate? #t]
                #:delta [delta 0]
                #:spike [spike 'sw]
                #:spike-dy [spike-dy #f]
                #:spike-dx [spike-dx #f]
                #:scale [b-scale 1]
                #:pin-find [pin-find (case spike
                                       [(e) lc-find]
                                       [(n) cb-find]
                                       [(s) ct-find]
                                       [(nw) cb-find]
                                       [(sw) ct-find]
                                       [(se) ct-find]
                                       [(w) rc-find])]
                #:width [width (current-para-width)]
                . content)
  (lambda (p)
    (define new-p
      (if color
          (refocus (cc-superimpose (cellophane
                                    (colorize (filled-rounded-rectangle (+ (+ 6 (pict-width p)) delta)
                                                                        (+ (- (pict-height p) 2) delta)
                                                                        4)
                                              color)
                                    (if animate? (animation-progress) 1))
                                   p)
                   p)
          p))
    (if (or (null? content)
            (and animate? (not (= 1 (animation-progress)))))
        new-p
        (refocus (pin-balloon (wrap-balloon (scale (apply para 
                                                          #:fill? #f 
                                                          #:width width
                                                          content) 
                                                   b-scale)
                                            spike 
                                            (or spike-dx
                                                (case spike
                                                  [(sw nw se n s) 0]
                                                  [(e) gap-size]
                                                  [(w) (- gap-size)]))
                                            (or spike-dy
                                                (case spike
                                                  [(nw n) (- gap-size)]
                                                  [(sw se s) gap-size]
                                                  [(w e) 0]))
                                            balloon-color
                                            16)
                              new-p
                              new-p pin-find)
                 new-p))))
    
(define (annote s p)
  (define lbl (rotate (colorize (bt s) "white") (/ pi 2)))
  (refocus (hc-append
            (/ gap-size 2)
            (cc-superimpose (colorize
                             (filled-rectangle (+ (pict-width lbl) 10)
                                               (pict-height p))
                             panel-color)
                            lbl)
            p)
           p))
