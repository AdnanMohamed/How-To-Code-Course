;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |3.6-Designing World Programs|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)

; An AnimationState is a Number.
; interpretation the number of clock ticks 
; since the animation started

; Physical constants
(define WIDTH-OF-WORLD 500)
(define WHEEL-RADIUS 5)
(define WHEEL-DISTANCE (* WHEEL-RADIUS 5))
(define HEIGHT-OF-WORLD (* 6 WHEEL-RADIUS))
(define CAR-COLOR "red")
(define CAR-SPEED 6)
(define CAR-ACC 2)


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

(define CAR (underlay/offset (above CAR-PART2 CAR-PART1) 0 WHEEL-RADIUS BOTH-WHEELS))

(define Y-CAR (image-height CAR))


; WorldState -> WorldState 
; moves the car by 3 pixels for every clock tick
;(check-expect (tock 5) 8)
;(check-expect (tock 0) 3)

(define (tock cw)
  (+ cw CAR-SPEED))

; WorldState -> Image
; places the car into the BACKGROUND scene,
; according to the given world state 
(define (render cw)
  (place-image CAR cw Y-CAR BACKGROUND))

; WorldState -> boolean
; ends the program when the car disappears
; from the right end of the screen
 (define (end? cw)
   (> (- cw (image-width CAR)) WIDTH-OF-WORLD))

; WorldState Number Number String -> WorldState
; places the car at x-mouse
; if the given me is "button-down" 
; given: 21 10 20 "enter"
; wanted: 21
; given: 42 10 20 "button-down"
; wanted: 10
; given: 42 10 20 "move"
; wanted: 42
 (check-expect (hyper 21 10 20 "enter") 21)
 (check-expect (hyper 42 10 20 "button-down") 10)
 (check-expect (hyper 42 10 20 "move") 42)
(define (hyper x-position-of-car x-mouse y-mouse me)
  (cond
    [(string=? "button-down" me) x-mouse]
    [else x-position-of-car]))

; WorldState -> WorldState
; launches the program from some initial state 
(define (main ws)
   (big-bang ws
     [on-tick tock]
     [on-mouse hyper]
     [to-draw render]
     [stop-when end?]))


(main 1)