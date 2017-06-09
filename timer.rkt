#lang racket/gui

(define game-window                               ;Game window with required parameters like label, width etc.
  (new frame%
       [label "Andreas window"]
       [width 800]
       [height 600]))

(send game-window show #t)

(define angle 0)

(define (drawing-proc2 canvas dc)
  (let ((our-picture (make-object bitmap% "images/Projektexbild.png")))
    (send dc translate 300 200)
    (send dc rotate angle)
    (send dc draw-bitmap our-picture -150 -173)
    (send dc rotate (- angle))
    (send dc translate -300 -200)))

(define game-canvas
  (new canvas%
       [parent game-window]                  
       [paint-callback drawing-proc2]))

(define (refresh-graphics)
  (send game-window refresh))

(define (physics-update)
  (set! angle (+ angle 0.02)))

(define graphics-timer
  (new timer%
       [notify-callback refresh-graphics]))

(send graphics-timer start 16 #f)

(define physics-timer
  (new timer%
       [notify-callback physics-update]))

(send physics-timer start 8 #f)