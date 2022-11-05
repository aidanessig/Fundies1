;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname hw11p1-starter) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; == Homework 11, Problem 1 ==

; Consider the following definitions for a network of roads that contain blocks (of some length,
; in meters), a T intersection (in which a single street splits into two), and a 4-way stop
; (in which a single road splits into three).

(define-struct block [dist next])
(define-struct tint [left right])
(define-struct 4waystop [left right forward])

; A Street is one of:
; - "dead end"
; - (make-block PosReal Street)
; - (make-tint Street Street)
; - (make-4waystop Street Street Street)
; - Interpretation: a network of roads

(define ROAD-DONE "dead end")
(define ROAD-1 (make-block 1 ROAD-DONE))
(define ROAD-2 (make-block 2 ROAD-DONE))
(define ROAD-11 (make-block 1 ROAD-1))
              

(define ROAD-TINT-11
  (make-tint
   ROAD-1
   ROAD-1))

(define ROAD-4WS-121
  (make-4waystop
   ROAD-1
   (make-block
    2
    ROAD-TINT-11)
   ROAD-1))

(define ROAD-N1
  (make-block
   1
   (make-block
    1
    ROAD-4WS-121)))



; TODO #1: write the template for a Street.



; TODO #2: design the function road-dist that calculates the total distance across
; all roads in a supplied network. You have been supplied some tests for clarity.

#|
(check-expect
 (road-dist ROAD-DONE)
 0)

(check-expect
 (road-dist ROAD-2)
 2)

(check-expect
 (road-dist ROAD-11)
 2)

(check-expect
 (road-dist ROAD-N1)
 8)
|#



; TODO #3: design the function same-network? that determines if two road networks are the same.
; You have been supplied some tests for clarity.

#|
(check-expect
 (same-network? ROAD-11 ROAD-2)
 #false)

(check-expect
 (same-network? ROAD-2 ROAD-2)
 #true)

(check-expect
 (same-network? ROAD-N1 ROAD-N1)
 #true)
|#


