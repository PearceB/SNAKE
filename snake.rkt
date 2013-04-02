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

(define-struct worm (posn dir))
; A Worm is a structure: (make-worm Number Command)
; interp. (make-worm p d) means the worm is at position (p)
; on the grid and is going in the direction (d)

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
(define INITIAL-WORM (make-worm WORM-POS "down"))
(define INITIAL-GAME (make-game INITIAL-WORM))

;---------------
; Core Functions
;---------------

; Game Command -> Game
; move the worm based on the command

(define (change-dir w cmd)
  (cond
    [(key=? cmd "up") (make-game (make-worm (worm-posn (game-worm w)) "up"))]
    [(key=? cmd "down") (make-game (make-worm (worm-posn (game-worm w)) "down"))]
    [(key=? cmd "left") (make-game (make-worm (worm-posn (game-worm w)) "left"))]
    [(key=? cmd "right") (make-game (make-worm (worm-posn (game-worm w)) "right"))]
    [else w]))

; Game -> Game
; Update the game state each frame

(define (move gs)
  (let* ([z (game-worm gs)]
         [pos (worm-posn z)]
         [x (posn-x pos)]
         [y (posn-y pos)])
  (cond
    [(string=? (worm-dir z) "up") (make-game (make-worm (make-posn x (- y GRID)) "up"))]
    [(string=? (worm-dir z) "down") (make-game (make-worm (make-posn x (+ y GRID)) "down"))]
    [(string=? (worm-dir z) "left") (make-game (make-worm (make-posn (- x GRID) y) "left"))]
    [(string=? (worm-dir z) "right") (make-game (make-worm (make-posn (+ x GRID) y) "right"))]
    [else gs])))

;------------------
; Display Rendering
;------------------

; Graphical Constants
(define FIELD (rectangle FIELD-WIDTH FIELD-HEIGHT "solid" "white"))
(define WORM-HEAD (circle HEAD-RADIUS "solid" "red"))

; Game -> Scene
; render the worm-head on the screen
(define (render-worm_head gs)
  (place-image WORM-HEAD
               (posn-x (worm-posn (game-worm gs)))
               (posn-y (worm-posn (game-worm gs)))
               FIELD))

; Create the world
(big-bang INITIAL-GAME
          (on-tick move 0.1)
          (on-key change-dir)
          (to-draw render-worm_head))