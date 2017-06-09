#lang racket/gui

(define game-window                               ;Game window with required parameters like label, width etc.
  (new frame%
       [label "Andreas window"]
       [width 1200]
       [height 800]))

(send game-window show #t)                        ;Shows the window, #f for not showing.

(define xpos 0)
(define ypos 0)

(define (drawing-proc2 canvas dc)
  (let ((our-picture (make-object bitmap% "images/Projektexbild.png")))
    (send dc translate xpos ypos)
    (send dc draw-bitmap our-picture -150 -173)
    (send dc translate (- xpos) (- ypos))))

(define (drawing-proc1 canvas dc)
  (send dc translate xpos ypos)
  (send dc draw-text "1" 40 40)
  (send dc translate (- xpos) (- ypos)))

(define input-canvas%
  (class canvas%
    (init-field keyboard-handler
                mouse-handler)

    (define/override (on-char key-event)
      (keyboard-handler key-event))
    (define/override (on-event mouse-event)
      (mouse-handler mouse-event))
    (super-new)))

(define (mouse-proc mouse-event)
  1)

(define (key-proc key-event)
  (when (eq? (send key-event get-key-code) 'up)
    (set! ypos (- ypos 40))
    (send game-window refresh))
  (when (eq? (send key-event get-key-code) 'down)
    (set! ypos (+ ypos 40))
    (send game-window refresh))
  (when (eq? (send key-event get-key-code) 'left)
    (set! xpos (- xpos 40))
    (send game-window refresh))
  (when (eq? (send key-event get-key-code) 'right)
    (set! xpos (+ xpos 40))
    (send game-window refresh)))

(define our-canvas%
  (new input-canvas%
       [parent game-window]
       [paint-callback drawing-proc1]
       [mouse-handler mouse-proc]
       [keyboard-handler key-proc]))