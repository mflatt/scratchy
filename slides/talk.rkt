#lang slideshow
(require "parsing.rkt"
         "patterns.rkt"
         "stxparam.rkt"
         "compile-time.rkt"
         "implicit.rkt")

(define (part filename)
  (slide
   (tt filename)))

(parsing-slides #:title "Language Steps")
(parsing-examples-slides #:title "Language Examples")
(parsing-rationale-slides #:title "Language Steps")

(part "fish1.rkt")
(simple-pattern-slides)

(part "fish2.rkt")
; (slide (para #:fill? #f "semaphores," (tt "rename-out")))

(part "fish3.rkt")
(pattern-slides)

(part "fish4.rkt")
(define-syntax-slides)
(syntax-parse-slides)

(part "fish5.rkt")
(stxparam-slides)

(part "fish6.rkt")
; (slide (para #:fill? #f "more" (tt "syntax-parse")))

(part "fish7.rkt")
(implicit-slides)

(part "fish8.rkt")
(compile-time-slides)
(tone-slides)

(part "fish9.rkt")
(slide (para #:fill? #f (tt "#lang reader")))

(part "fish10.rkt")
(slide (para #:fill? #f (t "collections and") (tt "#lang")))

(part "fish11.rkt")
(slide (para #:fill? #f (t "parser and colorer")))


