#lang racket
(require "runtime.rkt"
         "images.rkt"
         "sync-task.rkt"
         "key.rkt"
         "method.rkt")

;; Set up aliases for methods, so that we don't write `send'
;; => syntax parameters (see "method.rkt")

(define-sprite duck
  [image duck-image]
  [y 100]
  [key-callback
   (key-handler
    [up (move-y 10) (turn-to 0)]
    [down (move-y -10) (turn-to 180)]
    [right (move-x 10) (turn-to 90)]
    [left (move-x -10) (turn-to 270)]
    [space (turn 10)]
    [+ (change-size #e0.1)]
    [- (change-size #e-0.1)])])

(define-sprite fish
  [image fish-image]
  [variable score 0]
  [task
   (forever
    (sleep 0.02)
    (forward 2)
    (turn (- (random 5) 2))
    (when (touches? aq)
      (set! score (+ 1 score))
      (say score))
    (when (touches? duck)
      (hush)
      (turn 180)
      (while (touches? duck))))]
  [task
   (forever
    (sleep 0.1)
    (change-size #e0.05)
    (sleep 0.1)
    (change-size #e-0.05))])

(define-sprite aq [image aq-image])

(run+ (list aq duck fish))
