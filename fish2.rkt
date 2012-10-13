#lang racket
(require "runtime.rkt"
         "images.rkt"
         "task.rkt")

;; Use `task' instead of creating threads directly
;; => simple pattern-matching macros

(define duck (new sprite% 
                  [image duck-image]
                  [y 100]
                  [key-callback
                   (lambda (down? key)
                     (when down?
                       (case key
                         [(up) (send duck move-y 10) (send duck turn-to 0)]
                         [(down) (send duck move-y -10) (send duck turn-to 180)]
                         [(right) (send duck move-x 10) (send duck turn-to 90)]
                         [(left) (send duck move-x -10) (send duck turn-to 270)]
                         [(#\space) (send duck turn 10)]
                         [(#\+) (send duck change-size #e0.1)]
                         [(#\-) (send duck change-size #e-0.1)])))]))

(define fish (new sprite% 
                  [image fish-image]))

(define aq (new sprite% [image aq-image]))

(define score 0)

(task
 (forever
  (sleep 0.02)
  (send fish forward 2)
  (send fish turn (- (random 5) 2))
  (when (send fish touches? aq)
    (set! score (+ 1 score))
    (send fish say score))
  (when (send fish touches? duck)
    (send fish hush)
    (send fish turn 180)
    (while (send fish touches? duck)))))

(task
 (forever
  (sleep 0.1)
  (send fish change-size #e0.05)
  (sleep 0.1)
  (send fish change-size #e-0.05)))

(run (list aq duck fish))
