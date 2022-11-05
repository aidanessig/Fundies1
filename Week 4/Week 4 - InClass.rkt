;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |Week 4 - InClass|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)

; draw-circle
; interpretation : draws a circle

(define (TEST-CIRCLE size)
  (circle size "solid" "orange"))

(define (test s)
  (big-bang s
       [to-draw TEST-CIRCLE]))


(define-struct child [parents dob date])

; A child is a (make-child String String Number)
; Interpretation : the make up of a child
; - parents of the child
; - dob represents the day child was born
; - date represents the year

(define myChild (make-child "Frank & Sally" "December 19th" 2025))

(define (child-temp kid)
  (... (child-parents kid)
       (child-dob kid)
       (child-date kid) ...))

