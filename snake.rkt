;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname snake) (read-case-sensitive #t) (teachpacks ((lib "image.ss" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.ss" "teachpack" "2htdp")))))
; SNAKE

(require 2htdp/image)
(require 2htdp/universe)

;-----------------
; Data Definitions
;-----------------

; A Command is one of
; - "up" move the worm head up
; - "down" move the worm head down
; - "left" move the worm head left
; - "right" move the worm head right

(define-struct worm (posn tail dir))
; A Worm is a structure: (make-worm Position List Command)
; interp. (make-worm p t d) means the worm is at position (p)
; on the grid, has a tail (t), and is going in the direction (d)

(define-struct game (worm food))
; A Game is a structure: (make-game Worm)
; interp. (make-game Worm) means that the state of the game
; is made up of the state of the Worm head and the Position of the food particle

;-------------------
; Physical Constants
;-------------------
(define GRID 20)
(define SCALE 30)
(define MAX (- GRID 1))
(define RADIUS (/ SCALE 2))
(define FIELD-WIDTH (* GRID SCALE))
(define FIELD-HEIGHT (* GRID SCALE))

(define WORM-POS (make-posn (* 2 SCALE) (* 2 SCALE)))
(define WORM-TAIL empty)
(define INITIAL-WORM (make-worm WORM-POS WORM-TAIL "down"))
(define INITIAL-GAME (make-game INITIAL-WORM (make-posn (* SCALE (+ 1 (random MAX)))
                                                        (* SCALE (+ 1 (random MAX))))))

;----------------------------
; Helper Function Definitions
;----------------------------

; Game -> Boolean
; Check to see if the worm is going off the top of the screen

(define (off-top gs)
  (<= (posn-x (worm-posn (game-worm gs))) RADIUS))

; Game -> Boolean
; Check to see if the worm is going off the bottom of the screen

(define (off-bottom gs)
  (>= (posn-x (worm-posn (game-worm gs))) (- FIELD-HEIGHT RADIUS)))

; Game -> Boolean
; Check to see if the worm is going off the left side of the screen

(define (off-left gs)
  (<= (posn-y (worm-posn (game-worm gs))) RADIUS))

; Game -> Boolean
; Check to see if the worm is going off the right side of the screen

(define (off-right gs)
  (>= (posn-y (worm-posn (game-worm gs))) (- FIELD-WIDTH RADIUS)))

; Position Game -> Boolean
; Stop the game when the worm has collided with its "tail"

(define (collision-tail posn gs)
  (and (= (posn-x posn) (posn-x (worm-posn (game-worm gs))))
           (= (posn-y posn) (posn-y (worm-posn (game-worm gs))))))

; Game -> Boolean
; Stop the game when the worm has collided with one of the "walls"

(define (detect-collision_wall gs)
  (or (off-top gs) (off-bottom gs) (off-left gs) (off-right gs)))

; Game List -> Boolean
; Check to see if the worm has collided with one of its tail segments

(define (detect-collision_tail gs list)
  (cond
    [(empty? list) false]
    [else (if (collision-tail (first list) gs)
            true
            (detect-collision_tail gs (rest list)))]))

; List -> List
; take a list and give back a list containing the previous list except the last entree
(check-expect (remove_last (list 1)) empty)
(check-expect (remove_last (list 1 2)) (list 1))

(define (remove_last list)
  (reverse (rest (reverse list))))

;--------------------------
; Core Function Definitions
;--------------------------

; Game List -> Boolean
; Combine the two collision detecters

(define (detect-collision gs)
  (or (detect-collision_wall gs) (detect-collision_tail gs (worm-tail (game-worm gs)))))

; Position Position -> Position
; Generate food only after the worm head has collided with the previous food head

(define (food-check-create p pos)
  (if (equal? p pos) (make-posn (* SCALE (+ 1 (random MAX)))
                                (* SCALE (+ 1 (random MAX)))) p))

; Game -> Position
; Create food at random positions on the FIELD assuming MAX is greater than 1

(define (food-create gs)
  (food-check-create (game-food gs) (worm-posn (game-worm gs))))
  
; Game Command -> Game
; move the worm based on the command

(define (change-dir gs cmd)
  (cond
    [(key=? cmd "up") (make-game (make-worm (worm-posn (game-worm gs)) (worm-tail (game-worm gs)) "up")
                                 (game-food gs))]
    [(key=? cmd "down") (make-game (make-worm (worm-posn (game-worm gs)) (worm-tail (game-worm gs)) "down")
                                   (game-food gs))]
    [(key=? cmd "left") (make-game (make-worm (worm-posn (game-worm gs)) (worm-tail (game-worm gs)) "left")
                                   (game-food gs))]
    [(key=? cmd "right") (make-game (make-worm (worm-posn (game-worm gs)) (worm-tail (game-worm gs)) "right")
                                    (game-food gs))]
    [else gs]))

; Game -> Game
; Update the game state each frame

(define (update gs)
  (let* ([z (game-worm gs)]
         [pos (worm-posn z)]
         [x (posn-x pos)]
         [y (posn-y pos)]
         [move  (cond
                  [(string=? (worm-dir z) "up") (make-posn x (- y SCALE))]
                  [(string=? (worm-dir z) "down") (make-posn x (+ y SCALE))]
                  [(string=? (worm-dir z) "left") (make-posn (- x SCALE) y)]
                  [(string=? (worm-dir z) "right") (make-posn (+ x SCALE) y)]
                  [else gs])]
         [new-tail
          (if (equal? (game-food gs) pos)
              (cons pos (worm-tail z))
              (if (empty? (worm-tail z))
                  empty
                  (cons pos (remove_last (worm-tail z)))))])
    (make-game (make-worm move new-tail (worm-dir z)) (food-create gs))))

;------------------
; Display Rendering
;------------------

; Graphical Constants
(define FIELD (rectangle FIELD-WIDTH FIELD-HEIGHT "solid" "white"))
(define WORM-HEAD (circle RADIUS "solid" "red"))
(define WORM-SEGMENT (circle RADIUS "solid" "green"))
(define FOOD (circle RADIUS "solid" "blue"))

; Game -> Image
; render the worm-head on the screen

(define (render-worm_head gs)
  (place-image WORM-HEAD
               (posn-x (worm-posn (game-worm gs)))
               (posn-y (worm-posn (game-worm gs)))
               FIELD))

; Position Image -> Image
; render a worm-tail segment on the screen

(define (render-worm_segment pos img)
  (place-image WORM-SEGMENT
               (posn-x pos)
               (posn-y pos)
               img))

; List Image -> Image
; render the worm-body on the screen

(define (render-worm_tail list img)
  (cond
    [(empty? list) img]
    [else (render-worm_segment (first list) (render-worm_tail (rest list) img))]))

; Game -> Scene
; render the whole game state

(define (render-gamestate gs)
  (place-image FOOD
               (posn-x (game-food gs))
               (posn-y (game-food gs))
               (render-worm_tail (worm-tail (game-worm gs)) (render-worm_head gs))))

; Game -> Scene
; Render the end game scene

(define (render-egame gs)
  (overlay/align "left" "bottom"
                 (text "LOL You Lost :D" 12 "black")
                 (render-gamestate gs)))

; Create the world

(big-bang INITIAL-GAME
          (on-tick update 0.1)
          (on-key change-dir)
          (to-draw render-gamestate)
          (stop-when detect-collision render-egame))