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

(define-struct worm (pos dir))
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

; Worm Command -> Worm
; move the worm based on the command

(define (move-worm-dir w cmd)
  (cond
    [(key=? cmd "up") (make-worm (- (posn-x (worm-pos w)) SCALE)
                                 "up")]
    [(key=? cmd "down") (make-worm (+ (posn-x (worm-pos w)) SCALE)
                                  "down")]
    [(key=? cmd "left") (make-worm (- (posn-y (worm-pos w)) SCALE)
                                  "left")]
    [(key=? cmd "right") (make-worm (+ (posn-y (worm-pos w)) SCALE)
                                    "right")]
    [else w]))

;------------------
; Display Rendering
;------------------

; Graphical Constants
(define FIELD (rectangle FIELD-WIDTH FIELD-HEIGHT "solid" "white"))
(define WORM-HEAD (circle HEAD-RADIUS "solid" "red"))

; Worm -> Scene
; render the worm-head on the screen
(define (render-worm_head wh)
  (place-image WORM-HEAD
               (posn-x wh)
               (posn-y wh)
               FIELD)

; Create the world
(big-bang INITIAL-GAME
          (on-tick udate)
          (on-key control-game)
          (to-draw render-worm))