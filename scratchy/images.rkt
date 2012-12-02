#lang racket/base
(require racket/draw
         slideshow/pict
         racket/runtime-path)

(provide duck-image
         fish-image
         aq-image)

(define-runtime-path duck-png "duck.png")

(define duck-image (read-bitmap duck-png))

(define fish-image
  (standard-fish #:direction 'right #:color "red" 80 60))

(define aq-image
  (colorize (filled-rectangle 300 300) "skyblue"))
