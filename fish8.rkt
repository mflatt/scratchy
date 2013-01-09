#lang s-exp "scratchy.rkt"

;; The "scratchy.rkt" language prevents abuse of `set!'
;; => `syntax-local-value' (see "scratchy.rkt" and "variable.rkt")

(define-sprite duck
  #:image duck-image
  #:y 100
  #:key up (move-y 10) (turn-to 0)
  #:key down (move-y -10) (turn-to 180)
  #:key right (move-x 10) (turn-to 90)
  #:key left (move-x -10) (turn-to 270)
  #:key space (turn 10)
  #:key + (change-size #e0.1)
  #:key - (change-size #e-0.1))

(define-sprite fish
  #:image fish-image
  #:variable score 0
  #:task
  (forever
   (sleep 0.02)
   (forward 2)
   (turn (- (random 5) 2))
   (when (touches? aq)
     ;; (set! duck 8) ; syntax error
     (set! score (+ 1 score))
     (say score))
   (when (touches? duck)
     (hush)
     (turn 180)
     (while (touches? duck))))
  #:task
  (forever
   (sleep 0.1)
   (change-size #e0.05)
   (sleep 0.1)
   (change-size #e-0.05)))

(define-sprite aq
  #:image aq-image)
