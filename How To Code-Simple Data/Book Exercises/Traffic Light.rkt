;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |Traffic Light|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)

; Physical constants
(define WIDTH-OF-WORLD 150)
(define HEIGHT-OF-WORLD (* 2 WIDTH-OF-WORLD))
(define TL_WIDTH  20)
(define TL_HEIGHT (* 2 TL_WIDTH))
(define LIGHT_RADIUS (- (/ TL_WIDTH 2) 3))
(define X-TL (/ WIDTH-OF-WORLD 2))
(define Y-TL (/ HEIGHT-OF-WORLD 2))


; Graphical constants

(define BACKGROUND
  (empty-scene WIDTH-OF-WORLD HEIGHT-OF-WORLD))




(define (TRAFFIC_LIGHT state)
  (cond [(= state 0)
         (overlay/align "middle" "top" (circle LIGHT_RADIUS "solid" "red")
                  (rectangle TL_WIDTH TL_HEIGHT "solid" "black"))]
        [(= state 1)
         (overlay/align "middle" "middle" (circle LIGHT_RADIUS "solid" "yellow")
                  (rectangle TL_WIDTH TL_HEIGHT "solid" "black"))]
        [else
         (overlay/align "middle" "bottom" (circle LIGHT_RADIUS "solid" "green")
                  (rectangle TL_WIDTH TL_HEIGHT "solid" "black"))]))

; TrafficLight -> TrafficLight
; yields the next state given current state s
;(check-expect (traffic-light-next "red") "green")
;(check-expect (traffic-light-next "yellow") "red")
;(check-expect (traffic-light-next "green") "yellow")

(define (traffic-light-next s)
  (cond
    [(string=? "red" s) "green"]
    [(string=? "green" s) "yellow"]
    [(string=? "yellow" s) "red"]))

;; Natural[1, 3] -> TrafficLight
;; produces a TrafficLight color corresponding to 1, 2 or 3
;;(check-expect (number->trafficLight 0) "red")
;;(check-expect (number->trafficLight 1) "yellow")
;;(check-expect (number->trafficLight 2) "green")

(define (number->trafficLight n)
    (cond
    [(= n 0) "red"]
    [(= n 1) "yellow"]
    [else    "green"]))

; WorldState -> WorldState 
; moves to the next state
;(check-expect (tock 5) 6)
;(check-expect (tock 0) 1)
(define (tock cw)
  (+ cw 1))

; WorldState -> Image
; places the car into the BACKGROUND scene,
; according to the given world state 

(define (render cw)
  (place-image (TRAFFIC_LIGHT (modulo cw 3)) X-TL Y-TL BACKGROUND))

; WorldState -> boolean
; ends the program when the car disappears
; from the right end of the screen
(define (end? cw)
   (> cw 25))

; WorldState -> WorldState
; launches the program from some initial state 
(define (main ws)
   (big-bang ws
     [on-tick tock 2]
     [to-draw render]
     [stop-when end?]))


(main 0)