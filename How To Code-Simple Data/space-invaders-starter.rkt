;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname space-invaders-starter) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/universe)
(require 2htdp/image)

;; Space Invaders


;; Constants:

(define WIDTH  300)
(define HEIGHT 500)

(define INVADER-X-SPEED 1.5)  ;speeds (not velocities) in pixels per tick
(define INVADER-Y-SPEED 1.5)
(define TANK-SPEED 2)
(define MISSILE-SPEED 10)

(define HIT-RANGE 10)

(define INVADE-RATE 100)

(define BACKGROUND (empty-scene WIDTH HEIGHT))

(define INVADER
  (overlay/xy (ellipse 10 15 "outline" "blue")              ;cockpit cover
              -5 6
              (ellipse 20 10 "solid"   "blue")))            ;saucer

(define INVADER-WIDTH/2  (/ (image-width INVADER) 2))

(define TANK
  (overlay/xy (overlay (ellipse 28 8 "solid" "black")       ;tread center
                       (ellipse 30 10 "solid" "green"))     ;tread outline
              5 -14
              (above (rectangle 5 10 "solid" "black")       ;gun
                     (rectangle 20 10 "solid" "black"))))   ;main body

(define TANK-HEIGHT/2 (/ (image-height TANK) 2))
(define TANK-WIDTH/2  (/ (image-width  TANK) 2))

(define MISSILE (ellipse 5 15 "solid" "red"))
(define MISSILE-HEIGHT/2 (/ (image-height MISSILE) 2))


;; Data Definitions:

(define-struct game (invaders missiles tank))
;; Game is (make-game  (listof Invader) (listof Missile) Tank)
;; interp. the current state of a space invaders game
;;         with the current invaders, missiles and tank position

;; Game constants defined below Missile data definition

#;
(define (fn-for-game s)
  (... (fn-for-loinvader (game-invaders s))
       (fn-for-lom (game-missiles s))
       (fn-for-tank (game-tank s))))



(define-struct tank (x dir))
;; Tank is (make-tank Number Integer[-1, 1])
;; interp. the tank location is x, HEIGHT - TANK-HEIGHT/2 in screen coordinates
;;         the tank moves TANK-SPEED pixels per clock tick left if dir -1, right if dir 1

(define T0 (make-tank (/ WIDTH 2) 1))   ;center going right
(define T1 (make-tank 50 1))            ;going right
(define T2 (make-tank 50 -1))           ;going left

#;
(define (fn-for-tank t)
  (... (tank-x t) (tank-dir t)))



(define-struct invader (x y dx))
;; Invader is (make-invader Number Number Number)
;; interp. the invader is at (x, y) in screen coordinates
;;         the invader along x by dx pixels per clock tick

(define I1 (make-invader 150 100 12))           ;not landed, moving right
(define I2 (make-invader 150 HEIGHT -10))       ;exactly landed, moving left
(define I3 (make-invader 150 (+ HEIGHT 10) 10)) ;> landed (disappeared from bottom), moving right


#;
(define (fn-for-invader invader)
  (... (invader-x invader) (invader-y invader) (invader-dx invader)))


(define-struct missile (x y))
;; Missile is (make-missile Number Number)
;; interp. the missile's location is x y in screen coordinates

(define M1 (make-missile 150 300))                       ;not hit I1
(define M2 (make-missile (invader-x I1) (+ (invader-y I1) 10)))  ;exactly hit I1
(define M3 (make-missile (invader-x I1) (+ (invader-y I1) 15)))  ;> hit I1

#;
(define (fn-for-missile m)
  (... (missile-x m) (missile-y m)))



(define G0 (make-game empty empty T0))
(define G1 (make-game empty empty T1))
(define G2 (make-game (list I1) (list M1) T1))
(define G3 (make-game (list I1 I2) (list M1 M2) T1))
(define G4 (make-game (list I1 I2 I3) (list M1 M2) T1))

;; Game->Game
;; starts big-bang with G0
(define (main g)
  (big-bang g
    (on-tick   next-state)
    (to-draw   render-game)
    (on-key    key-handler)
    (stop-when end-game?)))

;; Game -> Game
;; produce a ticked and filtered version of the game
(check-expect (next-state G0)
              (make-game empty empty
                         (make-tank (+ (tank-x T0) TANK-SPEED) 1)))

(check-expect (next-state G1)
              (make-game empty empty
                         (make-tank (+ (tank-x T1) TANK-SPEED) 1)))

(check-expect (next-state G2)
              (ticked-game
               (make-game
                (filter-invaders (game-invaders G2) (game-missiles G2))
                (filter-missiles (game-missiles G2))
                (game-tank G2))))

(check-expect (next-state G3)
              (ticked-game
               (make-game
                (filter-invaders (game-invaders G3) (game-missiles G3))
                (filter-missiles (game-missiles G3))
                (game-tank G3))))

;(define (next-state g) g)    ;stub
(define (next-state g)
  (ticked-game
   (make-game (filter-invaders (game-invaders g) (game-missiles g))
              (filter-missiles (game-missiles g))
              (game-tank g))))

;; Game -> Game
;; Executes the next move of each part in the game
(check-expect (ticked-game G0)
              (make-game empty empty
                         (make-tank (+ (tank-x T0) TANK-SPEED) 1)))
(check-expect (ticked-game G1)
              (make-game empty empty
                         (make-tank (+ (tank-x T1) TANK-SPEED) 1)))

(check-expect (ticked-game G2)
              (make-game
               (move-invaders (game-invaders G2))
               (move-missiles (game-missiles G2))
               (make-tank (+ (tank-x T1) TANK-SPEED) 1)))

(check-expect (ticked-game G3)
              (make-game
               (move-invaders (game-invaders G3))
               (move-missiles (game-missiles G3))
               (make-tank (+ (tank-x T1) TANK-SPEED) 1)))

;(define (ticked-game g) g)   ;stub
(define (ticked-game s)
  (make-game
   (if (zero? (random INVADE-RATE))
       (append (list (make-invader (random WIDTH) 0 (random 15)))
               (move-invaders (game-invaders s)))
       (move-invaders (game-invaders s)))
   (move-missiles (game-missiles s))
   (move-tank (game-tank s))))


;; listOfInvader listOfMissile -> listOfInvader
;; removes the invaders which are shot by some missile
(check-expect (filter-invaders empty empty) empty)
(check-expect (filter-invaders empty (list M1 M2)) empty)
(check-expect (filter-invaders (list I1) empty) (list I1))
(check-expect (filter-invaders (list I1 I2) (list M1)) (list I1 I2))
(check-expect (filter-invaders (list I1 I2) (list M1 M2)) (list I2))
(check-expect (filter-invaders (list I2 I1) (list M1 M2)) (list I2))
(check-expect (filter-invaders (list I2 I1) (list M2 M1)) (list I2))
                              
;(define (filter-invaders loi lom) loi)  ;stub

(define (filter-invaders loi lom)
  (cond [(empty? loi) empty]
        [(empty? lom) loi]
        [else
         (if (any-hit? (first loi) lom)
             (filter-invaders (rest loi) lom)
             (append (list (first loi)) (filter-invaders (rest loi) lom)))]))

;; Invader listOfMissile -> Boolean
;; produces true iff at least one missile is within the range of hitting the invader
(check-expect (any-hit? I1 empty) false)         ; for sure I1 is not shot by nothing :)
(check-expect (any-hit? I2 empty) false)         ; similarly for I2 (and for all any other invader)
(check-expect (any-hit? I1 (list M1)) false)     ; M1 does not hit I1
(check-expect (any-hit? I1 (list M1 M2)) true)   ; M1 does not hit I1 but M2 does
(check-expect (any-hit? I1 (list M2 M1)) true)   ; order of missiles should not matter
(check-expect (any-hit? I1 (list M1 M3)) false)  ; Neither M1 nor M3 hits I1
(check-expect (any-hit? I1 (list M3 M1)) false)  ; order of missiles should not matter

;(define (any-hit? invader lom) false)  ;stub

(define (any-hit? invader lom)
  (cond [(empty? lom) false]
        [else
         (or (hit? invader (first lom))
             (any-hit? invader (rest lom)))]))

;; Invader Missile -> Boolean
;; returns true iff the m(issile) is within the range of hitting the i(nvader)
(check-expect (hit? I1 M1) false)
(check-expect (hit? I1 M2) true)
(check-expect (hit? I1 M3) false)

;(define (hit? i m) false)  ;stub
(define (hit? i m)
  (and (<= (abs (- (invader-x i) (missile-x m))) HIT-RANGE)
       (<= (abs (- (invader-y i) (missile-y m))) HIT-RANGE)))

;; listOfMissile -> listOfMissile
;; removes the missiles that went beyond the scope of the upper boundary
(check-expect (filter-missiles empty) empty)
(check-expect (filter-missiles (list (make-missile 120 -15))) empty)
(check-expect (filter-missiles (list (make-missile 30 -20)))  empty)
(check-expect (filter-missiles (list (make-missile 20 5))) (list (make-missile 20 5)))
(check-expect (filter-missiles (list (make-missile 48 65))) (list (make-missile 48 65)))

(check-expect (filter-missiles (list (make-missile 120 -15)
                                     (make-missile 30 -20)
                                     (make-missile 20 5)
                                     (make-missile 48 65)))
              (list (make-missile 20 5) (make-missile 48 65)))

(check-expect (filter-missiles (list (make-missile 20 5)
                                     (make-missile 120 -15)
                                     (make-missile 30 -20)
                                     (make-missile 48 65)))
              (list (make-missile 20 5) (make-missile 48 65)))
                                     
(check-expect (filter-missiles (list (make-missile 20 5)
                                     (make-missile 120 -15)                                     
                                     (make-missile 48 65)
                                     (make-missile 30 -20)))
              (list (make-missile 20 5) (make-missile 48 65)))

;(define (filter-missiles lom) lom)   ;stub

(define (filter-missiles lom)
  (cond [(empty? lom) empty]
        [else
         (if (filter-missile? (first lom))
             (filter-missiles (rest lom))
             (append (list (first lom)) (filter-missiles (rest lom))))]))

;; Missile -> Boolean
;; produces true if m is beyond the upper boundary
(check-expect (filter-missile? (make-missile 20 50)) false)
(check-expect (filter-missile? (make-missile 20 HEIGHT)) false)
(check-expect (filter-missile? (make-missile 20 (- 0 MISSILE-HEIGHT/2))) true)
(check-expect (filter-missile? (make-missile 70 (- 0 MISSILE-HEIGHT/2))) true)

;(define (filter-missile? m) false)  ;stub

(define (filter-missile? m)
  (<= (missile-y m) (- MISSILE-HEIGHT/2)))

;; listOfInvader -> listOfInvader
;; moves each invader to its new position
(check-expect (move-invaders empty) empty)


(check-expect (move-invaders (list I1))
              (append (list (make-invader
                             (+ (invader-x I1) (invader-dx I1)) (+ (invader-y I1) INVADER-Y-SPEED)
                             (invader-dx I1)))
                      empty))
(check-expect (move-invaders (list I1 I2))
              (append (list (make-invader
                             (+ (invader-x I1) (invader-dx I1)) (+ (invader-y I1) INVADER-Y-SPEED)
                             (invader-dx I1)))
                      (move-invaders (rest (list I1 I2)))))

;(define (move-invaders loi) loi)  ;stub

(define (move-invaders loi)
  (cond [(empty? loi) empty]
        [else
         (append (list (move-invader (first loi)))
                 (move-invaders (rest loi)))]))
        
;; Invader -> Invader
;; Moves the invader horizantally by (invader-dx i) and down by INVADER-Y-SPEED
;; If the invader reaches the right or left boundary then it should change its direction
;; and moves as diagonally in the opposite direction.
(check-expect (move-invader (make-invader 30 50 -7))                                   ; moving left, in-bound
              (make-invader (+ 30 -7) (+ 50 INVADER-Y-SPEED) -7))

(check-expect (move-invader (make-invader 58 45 7))                                    ; moving right, in-bound
              (make-invader (+ 58 7) (+ 45 INVADER-Y-SPEED) 7))


(check-expect (move-invader (make-invader (+ (- WIDTH INVADER-WIDTH/2 ) 4) 43 8))      ; moving right, passed right boundary
              (make-invader (- WIDTH INVADER-WIDTH/2 8) (+ 43 INVADER-Y-SPEED) -8))


(check-expect (move-invader (make-invader (- INVADER-WIDTH/2 1) 60 -5))                ; moving left, passed left boundary
              (make-invader (+ INVADER-WIDTH/2 5) (+ 60 INVADER-Y-SPEED) 5))

;(define (move-invader i) i)  ;stub

(define (move-invader invader)
  (if (> (invader-dx invader) 0)
      (if (> (+ (invader-x invader) INVADER-WIDTH/2) WIDTH)
          (make-invader
           (- WIDTH INVADER-WIDTH/2 (invader-dx invader))
           (+ (invader-y invader) INVADER-Y-SPEED)
           (* -1 (invader-dx invader)))
          (make-invader
           (+ (invader-x invader) (invader-dx invader))
           (+ (invader-y invader) INVADER-Y-SPEED)
           (invader-dx invader)))
      (if (< (- (invader-x invader) INVADER-WIDTH/2) 0)
          (make-invader
           (+ INVADER-WIDTH/2 (* -1 (invader-dx invader)))
           (+ (invader-y invader) INVADER-Y-SPEED)
           (* -1 (invader-dx invader)))
          (make-invader
           (+ (invader-x invader) (invader-dx invader))
           (+ (invader-y invader) INVADER-Y-SPEED)
           (invader-dx invader)))))

;; listOf Missile -> listOf Missile
;; moves each missile up by MISSILE-SPEED
(check-expect (move-missiles empty) empty)
(check-expect (move-missiles (list M1))
              (append (list (make-missile (missile-x M1) (- (missile-y M1) MISSILE-SPEED))) empty))
(check-expect (move-missiles (list M1 M2))
              (append (list (make-missile (missile-x M1) (- (missile-y M1) MISSILE-SPEED)))
                      (append (list (make-missile (missile-x M2) (- (missile-y M2) MISSILE-SPEED))) empty)))

(check-expect (move-missiles (list M3 M1 M2))
              (append (list (make-missile (missile-x M3) (- (missile-y M3) MISSILE-SPEED)))
                      (append (list (make-missile (missile-x M1) (- (missile-y M1) MISSILE-SPEED)))
                              (append (list (make-missile (missile-x M2) (- (missile-y M2) MISSILE-SPEED))) empty))))

;(define (move-missiles lom) lom)  ;stub

(define (move-missiles lom)
  (cond [(empty? lom) empty]
        [else
         (append (list (move-missile (first lom)))
                 (move-missiles (rest lom)))]))

;; Missile -> Missile
;; moves the missile up by MISSILE-SPEED
(check-expect (move-missile M1) (make-missile (missile-x M1) (- (missile-y M1) MISSILE-SPEED)))
(check-expect (move-missile M2) (make-missile (missile-x M2) (- (missile-y M2) MISSILE-SPEED)))
(check-expect (move-missile M3) (make-missile (missile-x M3) (- (missile-y M3) MISSILE-SPEED)))

;(define (move-missile m) m)   ;stub

(define (move-missile m)
  (make-missile (missile-x m) (- (missile-y m) MISSILE-SPEED)))

;; Tank -> Tank
;; Moves a tank to the left/right by TANK-SPEED
(check-expect (move-tank T0) (make-tank (+ (/ WIDTH 2) TANK-SPEED) 1))              ; dir= 1, right-boundary < WIDTH
(check-expect (move-tank T2) (make-tank (- (tank-x T2) TANK-SPEED) -1))             ; dir= -1, left-boundary > 0
(check-expect (move-tank (make-tank (- TANK-WIDTH/2 -1) 1))                         ; dir= 1,   left-boundary < 0
              (make-tank (+ (- TANK-WIDTH/2 -1) TANK-SPEED) 1))
(check-expect (move-tank (make-tank (+ 1 (- WIDTH TANK-WIDTH/2)) -1))               ; dir= -1, right-boundary > WIDTH
              (make-tank (- (+ 1 (- WIDTH TANK-WIDTH/2)) TANK-SPEED) -1))

(check-expect (move-tank (make-tank (- TANK-WIDTH/2 2) -1))                         ; dir= -1, left-boundary < 0
              (make-tank TANK-WIDTH/2 1))

(check-expect (move-tank (make-tank (+ (- WIDTH TANK-WIDTH/2) 3) 1))                ; dir= 1, right-boundary > WIDTH
              (make-tank (- WIDTH TANK-WIDTH/2) -1))

;(define (move-tank t) t)  ;stub

(define (move-tank t)
  (if (= (tank-dir t) 1)
      (if (> (+ (tank-x t) TANK-WIDTH/2) WIDTH)
          (make-tank (- WIDTH TANK-WIDTH/2) -1)
          (make-tank (+ (tank-x t) TANK-SPEED) 1))
      (if (< (- (tank-x t) TANK-WIDTH/2) 0)
          (make-tank TANK-WIDTH/2 1)
          (make-tank (- (tank-x t) TANK-SPEED) -1))))

;; Game -> Image
;; draws the tank, invaders, and all missiles on the screen
(check-expect (render-game G0)
              (place-images
               (list TANK)
               (list (make-posn (/ WIDTH 2) (- HEIGHT TANK-HEIGHT/2)))
               BACKGROUND))

(check-expect (render-game G1)
              (place-images
               (list TANK)
               (list (make-posn (tank-x T1) (- HEIGHT TANK-HEIGHT/2)))
               BACKGROUND))

(check-expect (render-game G2)
              (place-images
               (append (list INVADER)
                       (list MISSILE)
                       (list TANK))
               (append
                (list (make-posn (invader-x I1) (invader-y I1)))
                (list (make-posn (missile-x M1) (missile-y M1)))
                (list (make-posn (tank-x T1) (- HEIGHT TANK-HEIGHT/2))))
               BACKGROUND))

(check-expect (render-game G3)
              (place-images
               (append (list INVADER INVADER)
                       (list MISSILE MISSILE)
                       (list TANK))
               (append
                (list
                 (make-posn (invader-x I1) (invader-y I1))
                 (make-posn (invader-x I2) (invader-y I2)))
                (list
                 (make-posn (missile-x M1) (missile-y M1))
                 (make-posn (missile-x M2) (missile-y M2)))
                (list (make-posn (tank-x T1) (- HEIGHT TANK-HEIGHT/2))))
               BACKGROUND))

              

;(define (render-game g) empty-image)   ;stub

(define (render-game g)
  (place-images
   (append
    (invader-images (game-invaders g))
    (missile-images (game-missiles g))
    (list TANK))
   (append
    (invader-posns  (game-invaders g))
    (missile-posns  (game-missiles g))
    (list (tank-posn (game-tank g))))
   BACKGROUND))

;; listOfInvader -> listOfImages
;; a list containing 'n' invader images where 'n' is the number of invaders in loi
;; if no 'n' is zero, then empty is produced
(check-expect (invader-images empty) empty)
(check-expect (invader-images (list I1)) (list INVADER))
(check-expect (invader-images (list I1 I2)) (append (list INVADER) (append (list INVADER) empty)))
(check-expect (invader-images (list I1 I2 I3))
              (append (list INVADER)
                      (append (list INVADER)
                              (append (list INVADER) empty))))
;(define (invader-images loi) empty)  ;stub

(define (invader-images loi)
  (cond [(empty? loi) empty]
        [else
         (append (list INVADER)
                 (invader-images (rest loi)))]))

;; listOfMissile -> listOfImages
;; a list containing 'n' missile images where 'n' is the number of invaders in lom
;; if no 'n' is zero, then empty is produced
(check-expect (missile-images empty) empty)
(check-expect (missile-images (list M1)) (list MISSILE))
(check-expect (missile-images (list M1 M2))
              (append
               (list MISSILE)
               (append
                (list MISSILE)
                empty)))
(check-expect (missile-images (list M1 M2 M3))
              (append (list MISSILE)
                      (append (list MISSILE)
                              (append (list MISSILE) empty))))

;(define (missile-images lom) empty)  ;stub
(define (missile-images lom)
  (cond [(empty? lom) empty]
        [else
         (append (list MISSILE)
                 (missile-images (rest lom)))]))


;; listOfInvader -> listOfPosn
;; produces a list of positions where each element corresponds to the
;; position of the invader in loi. If loi is empty then empty is produced
(check-expect (invader-posns empty) empty)
(check-expect (invader-posns (list I1))
              (list (make-posn (invader-x I1) (invader-y I1))))
(check-expect (invader-posns (list I1 I2))
              (append
               (list (make-posn (invader-x I1) (invader-y I1)))
               (append (list (make-posn (invader-x I2) (invader-y I2)))
                       empty)))
(check-expect (invader-posns (list I1 I2 I3))
              (append (list (make-posn (invader-x I1) (invader-y I1)))
                      (append
                       (list (make-posn (invader-x I2) (invader-y I2)))
                       (append (list (make-posn (invader-x I3) (invader-y I3)))
                               empty))))

;(define (invader-posns loi) empty)   ;stub
(define (invader-posns loi)
  (cond [(empty? loi) empty]
        [else
         (append (list (make-posn (invader-x (first loi)) (invader-y (first loi))))
                 (invader-posns (rest loi)))]))


;; listOfMissile -> listOfPosn
;; produces a list of positions where each element corresponds to the
;; position of the missile in lom. If lom is empty then empty is produced
(check-expect (missile-posns empty) empty)
(check-expect (missile-posns (list M1))
              (list (make-posn (missile-x M1) (missile-y M1))))
(check-expect (missile-posns (list M1 M2))
              (append
               (list (make-posn (missile-x M1) (missile-y M1)))
               (append (list (make-posn (missile-x M2) (missile-y M2)))
                       empty)))
(check-expect (missile-posns (list M1 M2 M3))
              (append (list (make-posn (missile-x M1) (missile-y M1)))
                      (append
                       (list (make-posn (missile-x M2) (missile-y M2)))
                       (append (list (make-posn (missile-x M3) (missile-y M3)))
                               empty))))

;(define (missile-posns lom) empty)  ;stub

(define (missile-posns lom)
  (cond [(empty? lom) empty]
        [else
         (append (list (make-posn (missile-x (first lom)) (missile-y (first lom))))
                 (missile-posns (rest lom)))]))

;; Tank -> Posn
;; produces the position of the given tank
(check-expect (tank-posn T0) (make-posn (tank-x T0) (- HEIGHT TANK-HEIGHT/2)))
(check-expect (tank-posn T1) (make-posn (tank-x T1) (- HEIGHT TANK-HEIGHT/2)))
(check-expect (tank-posn T2) (make-posn (tank-x T2) (- HEIGHT TANK-HEIGHT/2)))

;(define (tank-posn t) (make-posn 0 0))

(define (tank-posn t)
  (make-posn (tank-x t) (- HEIGHT TANK-HEIGHT/2)))

;; Game KeyEvent -> Game
;; If the player presses space then a missile is created.
(check-expect (key-handler G0 "a") G0)
(check-expect (key-handler G0 " ")
              (make-game
               (game-invaders G0)
               (append
                (list (make-missile (tank-x T0) (- HEIGHT (* 2 TANK-HEIGHT/2))))
                (game-missiles G0))
               (game-tank G0)))
(check-expect (key-handler G1 " ")
              (make-game
               (game-invaders G1)
               (append
                (list (make-missile (tank-x T1) (- HEIGHT (* 2 TANK-HEIGHT/2))))
                (game-missiles G1))
               (game-tank G1)))

;(define (key-handler g ke) g)   ;stub

(define (key-handler g ke)
  (if (string=? ke " ")
      (make-game
       (game-invaders g)
       (append
        (list (make-missile (tank-x (game-tank g)) (- HEIGHT (* 2 TANK-HEIGHT/2))))
        (game-missiles g))
       (game-tank g))
      g))


;; Game -> Boolean
;; Returns true iff an invader has invaded the tank's world
(check-expect (end-game? G0) false)
(check-expect (end-game? G1) false)
(check-expect (end-game? G2) false)
(check-expect (end-game? G3) true)
(check-expect (end-game? G4) true)

;(define (end-game? g) false)   ;stub

(define (end-game? g)
  (any-landed? (game-invaders g)))

;; listOfInvaders -> Boolean
;; produces true iff at least one invader has landed (i.e. its y-position == HEIGHT)
(check-expect (any-landed? empty) false)
(check-expect (any-landed? (list I1))    false)
(check-expect (any-landed? (list I2))    true)
(check-expect (any-landed? (list I3))    true)
(check-expect (any-landed? (list I1 I2)) true)
(check-expect (any-landed? (list I1 I3)) true)
(check-expect (any-landed? (list I2 I1)) true)
(check-expect (any-landed? (list I3 I1)) true)
(check-expect (any-landed? (list I1 I2)) true)
(check-expect (any-landed? (list I2 I3)) true)

;(define (any-landed? loi) false)   ;stub

(define (any-landed? loi)
  (cond
    [(empty? loi) false]
    [else
     (or (invader-landed? (first loi))
         (any-landed? (rest loi)))]))

;; Invader -> Boolean
;; produces true iff i have landed
(check-expect (invader-landed? I1)    false)
(check-expect (invader-landed? I2)    true)
(check-expect (invader-landed? I3)    true)

;(define (invader-landed? i) false)   ;stub

(define (invader-landed? i)
  (>= (invader-y i) HEIGHT))