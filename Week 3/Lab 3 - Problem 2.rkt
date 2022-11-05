;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |Lab 3 - Problem 2|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)

; == Lab 3, Problem 2 ==

; You are to design a program text-mover
; to display and manipulate text on a
; background. Your program should accept
; some phrase to show, as well as initial
; location and color (we only support three:
; red, black, or purple) - you should then
; display the phrase on the screen as described.

; When the user presses a mouse
; button, the program should move the text
; to the location that they clicked. When
; the user presses a key on the keyboard,
; the program should rotate colors.

; TODO #1: design the text-mover function;
; think through the arguments to the function,
; how you will represent the world state,
; and what handlers you need to support.
; - Hint A: since your state has multiple parts
;           that change, you'll need a structure
;           to hold them, but the parts themselves
;           might also be new.
; - Hint B: you've been provided some data definitions
;           below that will be quite useful

; TODO #2: Finish designing the data from #1;
; think ahead to make examples that are useful
; for testing such operations as changing location
; and color

; TODO #3: design your to-draw handler, making
; use of the template(s) you designed in #2.

; TODO #4: design your remaining handler(s),
; again following the appropriate template(s).
; - Hint #1: for the mouse, you'll want
;            to respond only to the "button-up"
;            event, which you can check using
;            the mouse=? function.
; - Hint #2: make sure to follow your templates,
;            which may involve breaking the handlers
;            into helper functions.


; A Position is a (make-posn Real Real)
; Interpretation: a 2D location


; A RedBlackPurple (RBP) is one of:
; - "red"
; - "black"
; - "purple"
; Interpretation: available font colors


(define-struct tm [str pos col])

; A TextMover (TM) is a (make-tm String Position RBP)
; - str is the text to be displayed
; - pos is the location of the text
; - col is the color of the text
; Interpretation: all the information needed for
; the text-mover application

(define xPos 0)
(define yPos 0)

(define (text-mover s0)
  (big-bang s0
    [to-draw scene]
    [on-mouse change]))

(define (scene s)
  (overlay/offset (text s 18 "black") xPos yPos (empty-scene 500 500)))

(define (change s x y me)
  (cond
    [(mouse=? me "button-down") "hello"]
    [else "c"])) 