;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |Week 3 - InClass|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; add-five: Number -> Number
; returns the number with five added

; add-five-template : Number -> Number
(define (add-five-template n)
  ( ... n ...))

(check-expect (add-five 10) 15)
(check-expect (add-five -2) 3)
(check-expect (add-five 3.5) 8.5)

(define (add-five n)
  (+ n 5))

; A NUMGRADE is an integer
; Interpretation: represents a students grade

(define NUMGRADE-0 0)
(define NUMGRADE-75 75)
(define NUMGRADE-95 95)

(check-expect (num->grade NUMGRADE-0) GRADE-F)
(check-expect (num->grade NUMGRADE-75) GRADE-C)
(check-expect (num->grade NUMGRADE-95) GRADE-A)

; numbergrade-temp : NUMGRADE -> ?
(define (numericgrade-temp ng)
  (... ng ...))

(define GRADE-A "A")
(define GRADE-B "B")
(define GRADE-C "C")
(define GRADE-D "D")
(define GRADE-F "F")

; lettergrade-temp : GRADE -> ?
(define (lettergrade-temp lg)
  (...
   (cond
     [(string=? lg GRADE-A) ...]
     [(string=? lg GRADE-B) ...]
     [(string=? lg GRADE-C) ...]
     [(string=? lg GRADE-D) ...]
     [(string=? lg GRADE-F) ...])))

; num->grade : value -> GRADE
; returns the grade equivalent of a number grade
(define (num->grade grade)
  (cond
    [(<= 90 grade) GRADE-A]
    [(<= 80 grade) GRADE-B]
    [(<= 70 grade) GRADE-C]
    [(<= 60 grade) GRADE-D]
    [else GRADE-F]))

; A TEMP is an integer
; Interpretation: represent a celcius temperature
(define TEMP-95 95)
(define TEMP-87.2 87.2)
(define TEMP-15 15)

; temperature-temp : TEMP -> ?
(define (temperature-temp TEMP)
  ( ... TEMP ... ))

; convert : value -> value
; returns the fahrenheit conversion of a celcius temperature
(define (convert temp)
  ( + (* temp 9/5) 32))

(check-expect (convert 3) 37.4)
(check-expect (convert 25) 77)
(check-expect (convert -10) 14)
