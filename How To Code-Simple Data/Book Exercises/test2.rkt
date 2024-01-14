;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname test2) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)

; Physical constants
(define WIDTH-OF-WORLD 200)
(define WHEEL-RADIUS 5)
(define WHEEL-DISTANCE (* WHEEL-RADIUS 5))
(define HEIGHT-OF-WORLD (* 6 WHEEL-RADIUS))
(define CAR-COLOR "red")

; Graphical constants

(define BACKGROUND
  (empty-scene WIDTH-OF-WORLD HEIGHT-OF-WORLD))

(define WHEEL
  (circle WHEEL-RADIUS "solid" "black"))

(define SPACE
  (rectangle (* 2 WHEEL-RADIUS) WHEEL-RADIUS "solid" "white"))

(define BOTH-WHEELS
  (beside/align "bottom" WHEEL SPACE WHEEL))

(define CAR-PART1
  (rectangle (* 8 WHEEL-RADIUS) (* 2 WHEEL-RADIUS) "solid" CAR-COLOR))

(define CAR-PART2
  (rectangle (* 4 WHEEL-RADIUS) WHEEL-RADIUS "solid" CAR-COLOR))


(define CAR (underlay/offset (above CAR-PART2 CAR-PART1) 0 7 BOTH-WHEELS))

(define Y-CAR (image-height CAR))

; WorldState -> Image
; places the car into the BACKGROUND scene,
; according to the given world state 
 (define (render cw)
   (place-image CAR cw Y-CAR BACKGROUND))

 (render 50)