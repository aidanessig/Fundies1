;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |Homework 2 - Problem 1|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; == Homework 2, Problem 1 ==

; The code below sums distances represented in nautical
; miles, kilometers, and meters. For example,
; (total-meters 1 2 3) computes the total
; distance of 1 nautical mile, 2 kilometers, and 3 meters,
; producing 1852 + 2000 + 3 = 3855 meters.

; total-meters : Number Number Number -> Number
; returns the total meter distance of 'x nautical miles'
; 'x kilometers' and 'x meters'

(check-expect (total-meters 1 2 3) 3855)
(check-expect (total-meters 3 2 10) 7566)
(check-expect (total-meters .7 1.2 55) 2551.4)

(define M-PER-NM 1852)
(define M-PER-KM 1000)

(define (total-meters nm km m)
  (+ (* nm M-PER-NM)
     (* km M-PER-KM)
     m))

; TODO: complete the missing steps of the Design Recipe...
; 1. Replace SIGNATURE-HERE with a signature for this function.
; 2. Replace PURPOSE-HERE with a signature for this function.
; 3. Replace TESTS-HERE with two tests: one
;    must correspond to the description above and
;    the other must be different and of your choosing.
