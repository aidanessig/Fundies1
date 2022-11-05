;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |Exam - Problem 2|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; == Exam 1, Problem 2 ==

; TODO #1: design the function full-name that accepts a person's
; first name (e.g., "Grace"), last name (e.g., "Hopper"), and
; desired ordering (either first name then last name, or last name
; then first name), and produces the appropriate full name (e.g.,
; "Grace Hopper" or "Hopper, Grace"). !!!!!NOTICE THE COMMA BETWEEN LAST, FIRST!!!!!

; A Order is a String represented by one of:
; - "first-last"
; - "last-first"
; Interpretation: the ordering of a name

(define Order-F-L "first-last")
(define Order-L-F "last-first")

; Order-temp : Order -> ?
(define (Order-temp o)
  (cond
    [(string=? Order-F-L o) ...]
    [(string=? Order-L-F o) ...]))

; full-name ; String String Order -> String
; Interpretation: returns the two input strings (first name &
; last name) in an order either "first last" or "last, first"
(define (full-name first last order)
  (cond
    [(string=? Order-F-L order) (string-append first " " last)]
    [(string=? Order-L-F order) (string-append last ", " first)]))

(check-expect (full-name "Aidan" "Essig" Order-F-L) "Aidan Essig")
(check-expect (full-name "Aidan" "Essig" "last-first") "Essig, Aidan")
(check-expect (full-name "Mark" "Martin" "first-last") "Mark Martin")
(check-expect (full-name "Mark" "Martin" Order-L-F) "Martin, Mark")

