#lang racket/base
(require racket/gui/base
         racket/class
         racket/math
         file/convertible
         slideshow/pict
         slideshow/balloon)

(provide run
         sprite%)

;; This module implements the Scratchy run-time system, which is a fairly
;; thin layer on top of Racket's GUI and drawing libraries. Also, Scratchy
;; tasks are implemented as Racket threads.

(define draw-bounds? #f)
(define draw-hit? #f)

(define (run sprites)
  (define f (new frame%
                 [label "Scratchy"]
                 [width 800]
                 [height 600]))
  (define c (new (class canvas%
                   (inherit get-client-size
                            refresh)
                   (super-new [parent f]
                              [paint-callback
                               (lambda (c dc)
                                 (define-values (w h) (get-client-size))
                                 (for ([sprite (in-list sprites)])
                                   (send sprite draw dc w h)
                                   (when draw-bounds?
                                     (define-values (x1 y1 x2 y2) (send sprite bounding-box))
                                     (send dc set-brush (make-brush #:style 'transparent))
                                     (send dc draw-rectangle 
                                           (+ x1 (quotient w 2)) (+ y1 (quotient h 2))
                                           (- x2 x1) (- y2 y1)))))])

                   (define/override (on-char e)
                     (define code (send e get-key-code))
                     (define down? (not (eq? code 'release)))
                     (define key (if down?
                                     code
                                     (send e get-key-release-code)))
                     (for ([sprite (in-list sprites)])
                       (send sprite key down? key))
                     (refresh))

                   (define/override (on-event e)
                     (define type (send e get-event-type))
                     (when (member type '(left-down left-up
                                                    middle-down middle-up
                                                    right-down right-up))
                       (define down? (member type '(left-down middle-down right-down)))
                       (define-values (w h) (send c get-client-size))
                       (define x (- (send e get-x) (quotient w 2)))
                       (define y (- (send e get-y) (quotient h 2)))
                       (for/or ([sprite (in-list (reverse sprites))])
                         (and (send sprite hits? x y)
                              (begin
                                (send sprite mouse down? x y)
                                #t)))
                       (refresh))))))

  (thread (lambda ()
            (let loop ()
              (sleep 0.03)
              (define continue (make-semaphore))
              (queue-callback
               (lambda ()
                 (define-values (w h) (send c get-client-size))
                 (when (for/fold ([update? #f]) ([sprite (in-list sprites)])
                         (or (send sprite tick w h)
                             update?))
                   (send c refresh))
                 (semaphore-post continue)))
              (semaphore-wait continue)
              (loop))))
  
  (send f show #t))

(define handler-thread (eventspace-handler-thread (current-eventspace)))

(define sprite%
  (class object%
    (init image
          [(init-x x) 0]
          [(init-y y) 0]
          [(init-s size) 1]
          [direction 90]
          [key-callback void]
          [mouse-callback void])

    (define on-key key-callback)
    (define on-mouse mouse-callback)

    (define-syntax-rule (as-event e ...)
      (call-as-event (lambda () e ...)))
    (define-syntax-rule (as-modify-event e ...)
      (as-event e ... (set! modified? #t)))

    (define/private (call-as-event thunk)
      (define t (current-thread))
      (if (eq? t handler-thread)
          (thunk)
          (begin
            (queue-callback (lambda () 
                              (thread-send t (thunk))))
            (thread-receive))))

    (define bm (read-bitmap (open-input-bytes (convert image 'png-bytes))))
    (define w (send bm get-width))
    (define h (send bm get-height))

    (define w/2 (* w 0.5))
    (define h/2 (* h 0.5))
    (define -w/2 (* w -0.5))
    (define -h/2 (* h -0.5))

    (define argb (make-bytes (* w h 4)))
    (send bm get-argb-pixels 0 0 w h argb)

    (define solid-pixels
      (for*/list ([i (in-range w)]
                  [j (in-range h)]
                  #:when (positive? (bytes-ref argb (* 4 (+ i (* j w))))))
        (cons i j)))
    (define num-solid-pixels (length solid-pixels))

    (define visible? #t)
    (define x init-x) ; h-position: positive is right
    (define y init-y) ; v-position: positive is up
    (define r direction) ; degrees; 0 is up, 90 is to the right
    (define s init-s) ; scale

    (define cr (cos (radians r)))
    (define sr (sin (radians r)))

    (define pixels-per-point (ceiling (/ s)))

    (define hx 0)
    (define hy 0)

    (define/private (radians r)
      (- (/ pi 2) (* r (/ pi 180))))

    (define modified? #f)

    (define/public (forward v)
      (as-modify-event 
       (set! x (+ x (* v cr)))
       (set! y (+ y (* v sr)))))

    (define/public (move-x v)
      (as-modify-event 
       (set! x (+ x v))))

    (define/public (move-y v)
      (as-modify-event 
       (set! y (+ y v))))

    (define/public (change-size v)
      (as-modify-event 
       (set-s! (+ s v))))

    (define/public (turn n)
      (as-modify-event
       (set-r! (+ n r))))

    (define/public (turn-to v)
      (as-modify-event
       (set-r! v)))

    (define/public (show)
      (as-event
       (unless visible?
         (set! visible? #t)
         (set! modified? #t))))

    (define/public (hide)
      (as-event
       (unless visible?
         (set! visible? #f)
         (set! modified? #t))))

    (define-syntax-rule (define-getter-setter var get set)
      (begin
        (define/public (get)
          (as-event var))
        (define/public (set v)
          (as-modify-event
           (set! var v)))))

    (define-getter-setter x get-x set-x)
    (define-getter-setter y get-y set-y)

    (define/private (set-r! v)
      (set! r v)
      (set! cr (cos (radians r)))
      (set! sr (sin (radians r))))

    (define/private (set-s! v)
      (unless (positive? s)
        (error 'set-s! "size must be positive"))
      (set! s v)
      (set! pixels-per-point (ceiling (/ s))))

    (define msg #f)

    (define/public (say s)
      (as-modify-event
       (set! msg
             (pip-wrap-balloon (text (format "~a" s) 'swiss 24) 
                               'sw 0 20 balloon-color 12))))

    (define/public (hush)
      (as-modify-event (set! msg #f)))

    (define/public (draw dc cw ch)
      (when visible?
        (define t (send dc get-transformation))
        (send dc translate (+ (quotient cw 2) x) (- (quotient ch 2) y))
        (send dc rotate (radians r))
        (send dc scale s s)
        (send dc draw-bitmap bm -w/2 -h/2)
        (when draw-hit?
          (send dc draw-rectangle hx hy 2 2))
        (send dc set-transformation t)
        (when msg
          (draw-pict msg dc 
                     (+ (quotient cw 2) x (* w 0.25))
                     (- (quotient ch 2) y (* h 0.25))))))

    (define/public (tick w h)
      (and modified?
           (set! modified? #f)
           #t))

    (define/public (key down? key)
      (thread
       (lambda ()
         (on-key down? key))))

    (define/private (transform px py)
      (if (and (zero? px) (zero? py))
          (values x (- y))
          (let* ([px (* px s)]
                 [py (* py s)]
                 [rpx (- (* px cr) (* py sr))]
                 [rpy (+ (* px sr) (* py cr))]
                 [px (+ rpx x)]
                 [py (- rpy y)])
            (values px py))))

    (define/private (reverse-transform mx my)
      ;; reverse transform on mx and my:
      (let* ([mx (- mx x)]
             [my (+ my y)]
             [rmx (- (* mx cr) (* my sr))]
             [rmy (+ (* mx sr) (* my cr))]
             [mx (/ rmx s)]
             [my (/ rmy s)])
        (values mx my)))

    (define/public (hits? mx my)
      (as-event (hits?/i mx my)))

    (define/public (hits?/i mx my)
      (let-values ([(mx my) (reverse-transform mx my)])
        (and (<= -w/2 mx w/2)
             (<= -h/2 my h/2)
             (let ()
               (define bx (max 0 (inexact->exact (floor (+ mx w/2)))))
               (define by (max 0 (inexact->exact (floor (+ my h/2)))))
               (if (= 1 pixels-per-point)
                   ;; each point hits about one pixel:
                   (positive? (bytes-ref argb
                                         (* 4 (+ (* (min by (sub1 h)) w)
                                                 (min bx (sub1 w))))))
                   ;; each point hits many bitmap pixels due to scaling:
                   (for*/or ([dx (in-range pixels-per-point)]
                             [dy (in-range pixels-per-point)])
                     (positive? (bytes-ref argb
                                           (* 4 (+ (* (min (+ by dy) (sub1 h))
                                                      w)
                                                   (min (+ bx dx) (sub1 w))))))))))))

    (define/public (mouse down? mx my)
      (let-values ([(mx my) (reverse-transform mx my)])
        (thread
         (lambda ()
           (on-mouse down? mx my)))))

    (define/public (bounding-box)
      (define-values (x1 y1) (transform -w/2 -h/2))
      (define-values (x2 y2) (transform w/2 h/2))
      (define-values (x3 y3) (transform w/2 -h/2))
      (define-values (x4 y4) (transform -w/2 h/2))
      (values (min x1 x2 x3 x4) (min y1 y2 y3 y4) 
              (max x1 x2 x3 x4) (max y1 y2 y3 y4)))
    
    (define/public (points-to-test)
      (* num-solid-pixels s))

    (define/public (is-visible?) visible?)

    (define/public (touches? other)
      (as-event 
       (and visible?
            (send other is-visible?)
            (if ((points-to-test) . < . (send other points-to-test))
                (touches?/i other)
                (send other touches?/i this)))))

    (define/public (touches?/i other)
      (define-values (x1 y1 x2 y2) (bounding-box))
      (define-values (x3 y3 x4 y4) (send other bounding-box))
      (and (or (<= x1 x3 x2)
               (<= x1 x4 x2)
               (<= x3 x2 x4))
           (or (<= y1 y3 y2)
               (<= y1 y4 y2)
               (<= y3 y2 y4))
           ;; bounding boxes overlap; check pixels
           (let ([dx -w/2]
                 [dy -h/2]
                 [pw (inexact->exact (floor s))])
             (for/or ([p (in-list solid-pixels)])
               (define i (car p))
               (define j (cdr p))
               (and (positive? (bytes-ref argb (* 4 (+ i (* j w)))))
                    (if (pw . < . 3)
                        ;; treat pixel as touching one point:
                        (let-values ([(mx my) (transform (+ i dx) (+ j dy))])
                          (send other hits?/i mx my))
                        ;; each pixel hits multiple points:
                        (for*/or ([ddx (in-range pw)]
                                  [ddy (in-range pw)])
                          (let-values ([(mx my) (transform (+ i dx (/ ddx s)) (+ j dy (/ ddy s)))])
                            (send other hits?/i mx my)))))))))
    
    (super-new)))
