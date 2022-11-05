;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |Week 5 - InClass|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)

; A silly is one of:
; - Number
; - (make-posn String Image) // posn is racket word meaning define-struct is not needed

; silly-temp : silly -> ?
(define (silly-temp s)
  (cond
    [(number? s) ...]
    [(posn? s) (... (posn-x s)
                    (posn-y s) ...)]))

; A TwoCars is a (make-twocars Number Number Number Number)
; Interpretation: Two cars racing towards each other
;  - x1 is the x-position of the first car in pixels from the left
;  - vx1 is the x-velocity of the first car in pixels/tick (driving to the right)
;  - x2 is the x-position of the second car in pixels from the left
;  - vx2 is the x-velocity of the second car in pixels/tick (driving to the left)
 
(define-struct twocars [x1 vx1 x2 vx2])
 

(define TWOCARS-1 (make-twocars 1 2 300 4))
(define TWOCARS-2 (make-twocars 8 2 12 4))
 
(define (twocars-temp tc)
  (... (twocars-x1 tc) ... (twocars-vx1 tc) ...
       (twocars-x2 tc) ... (twocars-vx2 tc) ...))

(define-struct wreck (x vx))
; A Wreck is a (make-wreck Number Number)
; - x is the x-position of the wreck in pixels from the left
; - vx is the x-velocity of the wreck in pixels/tick (driving to the right)
; Interpretation: the combined "car" after two cars hit

(define WRECK-1 (make-wreck 10 -2))
 
(define (wreck-temp w)
  (... (wreck-x w) ...
       (wreck-vx w) ...))

; A CollisionSim (CS) is one of:
; - TwoCars
; - Wreck
; Interpretation: either two cars racing towards each other,
; or the combined car after they've hit

(define CS-1 TWOCARS-1)
(define CS-2 WRECK-1)
 
(define (cs-temp cs)
  (...
   (cond
     [(twocars? cs) (twocars-temp cs)]
     [(wreck? cs) (wreck-temp cs)])))
 
(define (collision initial-cs)
  (get-final-position
   (big-bang initial-cs
     [to-draw draw-cs]
     [on-tick move-cs])))

; get-final-position : CS -> Number
; Returns the final position of the first car, or the wreck

(check-expect (get-final-position CS-1) 1)
(check-expect (get-final-position CS-2) 10)
 
(define (get-final-position cs)
  (cond
    [(twocars? cs) (twocars-x1 cs)]
    [(wreck? cs) (wreck-x cs)]))

(define CAR-1 (rectangle 10 5 "solid" "blue"))
(define CAR-2 (rectangle 10 5 "solid" "red"))
(define CAR-W (rectangle 10 5 "solid" "purple"))
(define Y-CAR 50)
(define BACKGROUND (empty-scene 600 400))

 ; draw-cs : CS -> Image
; Draws a collision simulation

(check-expect (draw-cs TWOCARS-1)
              (place-image CAR-1
                           1 Y-CAR
                           (place-image CAR-2 300 Y-CAR BACKGROUND)))
 
(check-expect (draw-cs WRECK-1)
              (place-image CAR-W
                           10 Y-CAR
                           BACKGROUND))
 
(define (draw-cs cs)
  (cond
    [(twocars? cs) (draw-twocars cs)]
    [(wreck? cs) (draw-wreck cs)]))

; draw-twocars : TwoCars -> Image
; Draws each of the cars

(check-expect (draw-twocars TWOCARS-1)
              (place-image CAR-1
                           1 Y-CAR
                           (place-image CAR-2 300 Y-CAR BACKGROUND)))
 
(define (draw-twocars tc)
  (place-image CAR-1
               (twocars-x1 tc)
               Y-CAR
               (place-image CAR-2 (twocars-x2 tc) Y-CAR BACKGROUND)))

; draw-wreck : Wreck -> Image
; Draws the wreck

(check-expect (draw-wreck WRECK-1)
              (place-image CAR-W
                           10 Y-CAR
                           BACKGROUND))
 
(define (draw-wreck w)
  (place-image CAR-W
               (wreck-x w) Y-CAR
               BACKGROUND))


(define ACCEL 0.1)

; move-cs : CS -> CS
; Moves the simulation

(check-expect (move-cs TWOCARS-1)
              (make-twocars 3 2.1
                            296 4.1))

(check-expect (move-cs TWOCARS-2)
              WRECK-1)
 
(check-expect (move-cs WRECK-1)
              (make-wreck 8 -2))
 
(define (move-cs cs)
  (cond
    [(twocars? cs) (if (collided? cs)
                       (tc->wreck cs)
                       (move-twocars cs))]
    [(wreck? cs) (move-wreck cs)]))

; collided? : TwoCars -> Boolean
; Determines if the cars collide this time step

(check-expect (collided? TWOCARS-1) false)
(check-expect (collided? TWOCARS-2) true)
 
(define (collided? tc)
  (>= (+ (twocars-x1 tc) (twocars-vx1 tc))
      (- (twocars-x2 tc) (twocars-vx2 tc))))

; tc->wreck : TwoCars -> Wreck
; Converts a two-car situation into a wreck
 
(check-expect (tc->wreck TWOCARS-2) WRECK-1)
 
(define (tc->wreck tc)
  (make-wreck
   (/ (+ (twocars-x1 tc) (twocars-x2 tc)) 2)
   (- (twocars-vx1 tc) (twocars-vx2 tc))))

; move-twocars : TwoCars -> TwoCars
; Moves each of the cars

(check-expect (move-twocars TWOCARS-1)
              (make-twocars 3 2.1
                            296 4.1))
 
(define (move-twocars tc)
  (make-twocars (+ (twocars-x1 tc) (twocars-vx1 tc))
                (+ (twocars-vx1 tc) ACCEL)
                (- (twocars-x2 tc) (twocars-vx2 tc))
                (+ (twocars-vx2 tc) ACCEL)))

; move-wreck : Wreck -> Wreck
; Moves the wreck

(check-expect (move-wreck WRECK-1)
              (make-wreck 8 -2))
 
(define (move-wreck w)
  (make-wreck
   (+ (wreck-x w) (wreck-vx w))
   (wreck-vx w)))

; collision : CS -> Number
; Simulate a car crash, returning the final position
; of the first car or the wreck
;(collision (make-twocars 1 1 500 2))

; draw-twocars : TwoCars -> Image
; Draws each of the cars

(define-struct moon [x vx])
 
; A Moon is a (make-moon Number Number)
; Representing the position and velocity of a moon.
;  - x is the x-position of the moon
;  - vx is the x-velocity of the moon
 
(define MOON-1 (make-moon 10 1))
(define MOON-2 (make-moon 300 -1))
 
(define (moon-temp m)
  (... (moon-x m) ... (moon-vx m) ...))
 
 
(define-struct system [moon system])
 
; A System is one of:
; - #false
; - (make-system Moon System)
; Representing the collection of moons we're trying
; to simulate (or #false for an empty system)
; - make-system
;  - moon is the current moon
;  - system is the rest of the moons
 
(define SYSTEM-0 #false)
(define SYSTEM-1 (make-system MOON-1 SYSTEM-0))
(define SYSTEM-2 (make-system MOON-2 SYSTEM-1))
 
(define (system-temp s)
  (...
   (cond
     [(boolean? s) ...]
     [(system? s) ...
      (moon-temp (system-moon s)) ...
      (system-temp (system-system s)) ...])))
 
; eclipse : System -> System
; Runs an eclipse of moons
 
(define (eclipse initial-s)
  (big-bang initial-s
    [to-draw draw-eclipse]
    [on-tick move-eclipse]
    [on-key add-moon]))
 
; draw-eclipse : System -> Image
; Draw the moons on a scene with the sun
 
(define SIZE 400)
(define HALF (/ SIZE 2))
 
(define SUN (circle (/ SIZE 10) "solid" "yellow"))
(define MOON (circle (/ SIZE 10) "solid" "gray"))
(define SKY (square SIZE "solid" "pink"))
 
(check-expect (draw-eclipse SYSTEM-0)
              (place-image
               SUN
               HALF HALF
               SKY))
 
(check-expect (draw-eclipse SYSTEM-2)
              (place-image
               MOON
               300 HALF
               (place-image
                MOON
                10 HALF
                (place-image
                 SUN
                 HALF HALF
                 SKY))))
 
(define (draw-eclipse s)
  (cond
    [(boolean? s) (place-image SUN HALF HALF SKY)]
    [(system? s)
     (draw-moon
      (system-moon s)
      (draw-eclipse (system-system s)))]))
 
; draw-moon : Moon Image -> Image
; Draws a moon onto a background
 
(check-expect (draw-moon MOON-1 SKY)
              (place-image MOON (moon-x MOON-1) HALF SKY))
 
(define (draw-moon moon background)
  (place-image MOON
               (moon-x moon) HALF
               background))
 
; move-eclipse : System -> System
; Moves moons for one tick
 
(check-expect (move-eclipse SYSTEM-0) SYSTEM-0)
 
(check-expect (move-eclipse SYSTEM-2)
              (make-system
               (make-moon 299 -1)
               (make-system
                (make-moon 11 1)
                #false)))
 
(define (move-eclipse s)
  (cond
    [(boolean? s) #false]
    [(system? s)
     (make-system
      (move-moon (system-moon s))
      (move-eclipse (system-system s)))]))
 
; move-moon : Moon -> Moon
; Moves a single moon
 
(check-expect (move-moon MOON-1)
              (make-moon 11 1))
 
(check-expect (move-moon MOON-2)
              (make-moon 299 -1))
 
(define (move-moon m)
  (make-moon (+ (moon-x m) (moon-vx m))
             (moon-vx m)))
 
; add-moon : System KeyEvent -> System
; Adds a moon to the system
 
(define NEW-MOON-X 1)
(define NEW-MOON-VX 20)
 
(check-expect (add-moon SYSTEM-0 " ")
              (make-system
               (make-moon NEW-MOON-X NEW-MOON-VX)
               #false))
 
(check-expect (add-moon SYSTEM-2 "right")
              (make-system
               (make-moon NEW-MOON-X NEW-MOON-VX)
               SYSTEM-2))
 
(define (add-moon s ke)
  (make-system (make-moon NEW-MOON-X NEW-MOON-VX) s))

(eclipse SYSTEM-0)