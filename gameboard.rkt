#lang racket/gui
(require "place.rkt")
(require racket/trace)

(define board-width-box 10)
(define board-height-box 10)
(define board-width-level 250)
(define board-height-level 10)

(define enumerate                
  (lambda (from to steps)        
    (if (> from to)
        '()
        (cons from (enumerate (+ from steps) to steps)))))


#|Sets up the worldview for Andreas, each "matrix" slot will be defined as air, obstacles enemies etc
that can roll in and change depending on it's pixel coordinates.|#

(define viewbox
  (new place%
   [name "Viewbox"]
   [description "The frame for what the player can see."]
   [board (build-vector
    board-width-box
    (lambda (i)
      (build-vector
       board-height-box
       (lambda (j)
         (vector
          (cond
            [(and (= i 9) (member j '(0 1 2 3 4 5 6 7 8 9)))  ;50 = obstacles
             50]
            [(and (= i 8) (member j '(0 1 2 3 4 5 6 7 8 9)))
             50]
            [(and (= i 5) (member j '(5)))                    ;90 = enemies
             90]
            [(and (= i 7) (member j '(4)))
             0]
            [else
             (modulo 1 10)]))))))]))

(define level1 
   (build-vector
    board-height-level
    (lambda (i)
      (build-vector
       board-width-level
       (lambda (j)
         (vector
          (cond
            [(and (= i 9) (member j (enumerate 0 250 1)))  ;50 = obstacles
             50]
            [(and (= i 8) (member j (enumerate 0 250 1)))
             50]
            [else
             (modulo 1 10)])))))))

#|Jump function that updates when Andreas jumps or not, null arguments lets him jump and "jump down" lets him fall".|#

(define move
  (let ([andreas-horiz 7]
        [andreas-verti 4])
    (lambda (args)
    (define check-enemy
      (lambda (args)
        (cond
          ((and (eq? args 'jump)
              (equal? (vector-ref (vector-ref viewbox (- andreas-horiz 1)) andreas-verti) #(90)))
           (error "Hit by an enemy!"))
          ((and (eq? args 'down)
              (equal? (vector-ref (vector-ref viewbox (+ andreas-horiz 1)) andreas-verti) #(90)))
           (displayln "ENEMY SLAIN!")
           (move-dir args)
           viewbox)
          ((and (eq? args 'right)
              (equal? (vector-ref (vector-ref viewbox andreas-horiz) (+ andreas-verti 1)) #(90)))
           (error "Hit by an enemy!"))
          ((and (eq? args 'left)
              (equal? (vector-ref (vector-ref viewbox andreas-horiz) (- andreas-verti 1)) #(90)))
           (error "Hit by an enemy!"))
          (else
           (move-dir args)))))
    (define move-dir
      (lambda (args)
        (cond
        ([eq? args 'jump]
         (vector-set! (vector-ref viewbox andreas-horiz) andreas-verti #(1))
         (vector-set! (vector-ref viewbox (- andreas-horiz 1)) andreas-verti #(0))
         (set! andreas-horiz (- andreas-horiz 1)))
        ([eq? args 'down]
         (cond
          ((equal? (vector-ref (vector-ref viewbox (+ andreas-horiz 1)) andreas-verti) #(50))
          (displayln "Can't move through obstacles")
          viewbox)
         (else
          (vector-set! (vector-ref viewbox andreas-horiz) andreas-verti #(1))
          (vector-set! (vector-ref viewbox (+ andreas-horiz 1)) andreas-verti #(0))
          (set! andreas-horiz (+ andreas-horiz 1)))))
        ([eq? args 'right]
         (vector-set! (vector-ref viewbox andreas-horiz) andreas-verti  #(1))
         (vector-set! (vector-ref viewbox andreas-horiz) (+ andreas-verti 1) #(0))
         (set! andreas-verti (+ andreas-verti 1)))
        ([eq? args 'left]
         (vector-set! (vector-ref viewbox andreas-horiz) andreas-verti #(1))
         (vector-set! (vector-ref viewbox andreas-horiz) (- andreas-verti 1) #(0))
         (set! andreas-verti (- andreas-verti 1)))
        (else
         (error "Unknown command")))))
      (check-enemy args))))











#|(define game-window                               ;Game window with required parameters like label, width etc.
  (new frame%
       [label "Andreas window"]
       [width 800]
       [height 800]))

(send game-window show #t) 



(define (drawing-proc1 canvas dc)
  (send dc set-font (make-object font% 20 "Courier" 'roman))
  (send dc draw-text "This is some test" 500 20)
  (send dc set-font (make-object font% 20 'default))
  (send dc draw-text "This is some test" 500 50)
  (send dc set-brush "red" 'solid)
  (send dc set-pen "blue" 3 'long-dash)
  (send dc draw-rectangle 100 50 100 100))

(define empty-bitmap (make-object bitmap% 300 300))

(define our-dc
  (new bitmap-dc%
       [bitmap empty-bitmap]))

(define (drawing-ground canvas dc)
  (send dc draw-rectangle 0 0 80 80)
  (send dc set-brush "brown" 'solid))

(define (drawing-sky canvas dc)
  (send dc draw-rectangle 400 400 80 80)
  (send dc set-brush "blue" 'solid))

(define (draw-map canvas dc)
  (drawing-ground canvas dc)
  (drawing-sky canvas dc))

(define game-canvas
  (new canvas%
       [parent game-window]                  
       [paint-callback draw-map]))|#

















    