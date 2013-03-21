;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname snake) (read-case-sensitive #t) (teachpacks ((lib "image.ss" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.ss" "teachpack" "2htdp")))))
; SNAKE

(require 2htdp/universe)

;-----------------
; Data Definitions
;-----------------

;-------------------
; Physical Constants
;-------------------
(define BALL-RADIUS 7)
(define FIELD-WIDTH 200)
(define FIELD-HEIGHT 200)
(define TO-LEFT -5)
(define TO-RIGHT 5)
(define WORM-DELTA 10) ;amount worm moves at a time

;------------------
; Display Rendering
;------------------

; Graphical Game
(define FIELD (rectangle FIELD-WIDTH FIELD-HEIGHT "solid" "white"))
(define WORM-HEAD (circle BALL-RADIUS "solid" "red"))

; Image -> Image
; render the worm-head on the screen
(define (render-worm_head wh)
  (place-image WORM-HEAD
               