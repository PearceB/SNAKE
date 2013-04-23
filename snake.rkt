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

(define-struct game (worm))
; A Game is a structure: (make-game Worm)
; interp. (make-game Worm) means that the state of the game
; is made up of the state of the Worm head

;-------------------
; Physical Constants
;-------------------
(define GRID 20)
(define SCALE 30)
(define HEAD-RADIUS (/ SCALE 2))
(define FIELD-WIDTH (* GRID SCALE))
(define FIELD-HEIGHT (* GRID SCALE))

(define WORM-POS (make-posn GRID SCALE))
(define INITIAL-WORM (make-worm WORM-POS empty "down"))
(define INITIAL-GAME (make-game INITIAL-WORM))

;----------------------------
; Helper Function Definitions
;----------------------------

; Game -> Boolean
; Check to see if the worm is going off the top of the screen

(define (off-top gs)
  (<= (posn-x (worm-posn (game-worm gs))) HEAD-RADIUS))

; Game -> Boolean
; Check to see if the worm is going off the bottom of the screen

(define (off-bottom gs)
  (>= (posn-x (worm-posn (game-worm gs))) (- FIELD-HEIGHT HEAD-RADIUS)))

; Game -> Boolean
; Check to see if the worm is going off the left side of the screen

(define (off-left gs)
  (<= (posn-y (worm-posn (game-worm gs))) HEAD-RADIUS))

; Game -> Boolean
; Check to see if the worm is going off the right side of the screen

(define (off-right gs)
  (>= (posn-y (worm-posn (game-worm gs))) (- FIELD-WIDTH HEAD-RADIUS)))

; List -> List
; take a list and give back a list containing the previous list except the last entree
(define (remove_last list)
  (reverse (rest (reverse list))))

;--------------------------
; Core Function Definitions
;--------------------------

; Game -> Boolean
; Stop the game when the worm has collided with one of the "walls"

(define (detect-collision gs)
  (if (or (off-top gs) (off-bottom gs) (off-left gs) (off-right gs))
      true
      false))

; Game Command -> Game
; move the worm based on the command

(define (change-dir w cmd)
  (cond
    [(key=? cmd "up") (make-game (make-worm (worm-posn (game-worm w)) empty "up"))]
    [(key=? cmd "down") (make-game (make-worm (worm-posn (game-worm w)) empty "down"))]
    [(key=? cmd "left") (make-game (make-worm (worm-posn (game-worm w)) empty "left"))]
    [(key=? cmd "right") (make-game (make-worm (worm-posn (game-worm w)) empty "right"))]
    [else w]))

; Game -> Game
; Update the game state each frame

(define (move gs)
  (let* ([z (game-worm gs)]
         [pos (worm-posn z)]
         [x (posn-x pos)]
         [y (posn-y pos)]
         [update  (cond
                    [(string=? (worm-dir z) "up") (make-game (make-worm (make-posn x (- y GRID)) empty "up"))]
                    [(string=? (worm-dir z) "down") (make-game (make-worm (make-posn x (+ y GRID)) empty "down"))]
                    [(string=? (worm-dir z) "left") (make-game (make-worm (make-posn (- x GRID) y) empty "left"))]
                    [(string=? (worm-dir z) "right") (make-game (make-worm (make-posn (+ x GRID) y) empty "right"))]
                    [else gs])]
         [new-tail
          (if (empty? (worm-tail z))
              empty
              (cons pos (remove_last (worm-tail z))))])
    (make-game (make-worm pos new-tail (worm-dir z)))))

;------------------
; Display Rendering
;------------------

; Graphical Constants
(define FIELD (rectangle FIELD-WIDTH FIELD-HEIGHT "solid" "white"))
(define WORM-HEAD (circle HEAD-RADIUS "solid" "red"))
(define WORM-TAIL (circle HEAD-RADIUS "solid" "green"))

; Game -> Scene
; render the worm-head on the screen
(define (render-worm_head gs)
  (place-image WORM-HEAD
               (posn-x (worm-posn (game-worm gs)))
               (posn-y (worm-posn (game-worm gs)))
               FIELD))

; Game -> Scene
; render a worm-tail segment on the screen
(define (render-worm_tail gs)
  (place-image WORM-TAIL
               10
               10
               FIELD))

; Game -> Scene
; render the worm-body on the screen
(define (render-worm_body tail gs)
  (cond
    [(empty? (worm-tail (game-worm gs))) (render-worm_head gs)]
    [else (render-worm_tail WORM-TAIL (first tail) (render-tail (rest tail) img))]))

; Game -> Scene
; Render the end game scene
(define (render-egame gs)
  (place-image WORM-HEAD
               (posn-x (worm-posn (game-worm gs)))
               (posn-y (worm-posn (game-worm gs)))
               (overlay/align "left" "bottom"
                              (text "Worm Hit Border" 12 "black")
                              FIELD)))

; Create the world
(big-bang INITIAL-GAME
          (on-tick move 0.1)
          (on-key change-dir)
          (to-draw render-worm_body)
          (stop-when detect-collision render-egame))