;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |Homework 3 - Problem 3|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)

; == Homework 3, Problem 3 ==

; TODO: design the World program bit-face to allow
; someone to customize a face based upon a "menu"
; of options for eyes, mouth, and hair.
;
; You should have (at least) three options for eyes,
; three options for mouth, and three options for hair.
; Each time the user presses the "e" key, they should
; see the next option for eyes (cycling as necessary);
; same with "m" for mouth and "h" for hair. The
; bit-face function should supply initial values for
; each of these face features.
;
; You may draw these or use images from the web, but
; any combination (of eyes/mouth/hair) should be possible.
; You should NOT have a single representation of all
; possible combinations (since this will make it hard to
; add future options, such as ears, jewelry, makeup, ...).
; Instead, design data for each of the changing features.
; Be creative, but respectful :)

; As always, your functions should follow appropriate
; templates.

(define-struct FACE [eyes hair mouth])

; A FACE is a (make-FACE String String String)
; Interpretation: draws the users colors on a face
(define FACE-1 (make-FACE "blue" "purple" "yellow"))
(define FACE-2 (make-FACE "brown" "red" "blue"))
(define FACE-3 (make-FACE "black" "blue" "white"))


(define HEAD (circle 50 "solid" "green"))
(define (EYES color) (overlay/xy (circle 15 "solid" color)
                                 40 0
                                 (circle 15 "solid" color)))
(define (HAIR color) (isosceles-triangle 75 125 "solid" color))
(define (MOUTH color) (rectangle 40 20 "solid" color))

; bit-face : FACE -> FACE
; takes a FACE which can be changed based
; on user input
(define (bit-face s)
  (big-bang s
    [to-draw draw-scene]
    [on-key change]))

; draw-scene : FACE -> FACE
; draws the inputed FACE in a big-bang window
(define (draw-scene s)
  (overlay (overlay/offset (MOUTH (FACE-mouth s))
                           0 -30
                           (overlay/offset (HAIR (FACE-hair s))
                                           0 45
                                           (overlay/offset (EYES (FACE-eyes s))
                                                           0 10
                                                           HEAD))) (empty-scene 150 150)))

; change : FACE key -> FACE
; changes FACE based on the pressed key
(define (change face key)
  (cond
    [(key=? "e" key) (change-eyes face)]
    [(key=? "h" key) (change-hair face)]
    [(key=? "m" key) (change-mouth face)]))

; change-eyes : FACE -> FACE
; changes eyes by calling increment-eyes 
(define (change-eyes e)
  (make-FACE (increment-eyes (FACE-eyes e)) (FACE-hair e) (FACE-mouth e)))

; increment-eyes : eyes -> eyes
; increments to next color
(define (increment-eyes e)
  (cond
    [(string=? e "blue") "black"]
    [(string=? e "black") "red"]
    [(string=? e "red") "blue"]
    [else "blue"]))

; change-hair : FACE -> FACE
; changes hair by calling increment-hair
(define (change-hair e)
  (make-FACE (FACE-eyes e) (increment-hair (FACE-hair e)) (FACE-mouth e)))

; increment-hair : hair -> hair
; increments to next color
(define (increment-hair h)
  (cond
    [(string=? h "purple") "orange"]
    [(string=? h "orange") "brown"]
    [(string=? h "brown") "purple"]
    [else "purple"]))

; change-mouth : FACE -> FACE
; changes mouth by calling increment-mouth 
(define (change-mouth e)
  (make-FACE (FACE-eyes e) (FACE-hair e) (increment-mouth (FACE-mouth e))))

; increment-mouth : mouth -> mouth
; increments to next color
(define (increment-mouth m)
  (cond
    [(string=? m "yellow") "white"]
    [(string=? m "white") "black"]
    [(string=? m "black") "yellow"]
    [else "yellow"]))

