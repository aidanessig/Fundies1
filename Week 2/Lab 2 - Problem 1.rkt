;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |Lab 2 - Problem 1|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; == Lab 2, Problem 1 ==

; TODO: Design the function string-starts-with? which takes two
; Strings and returns a Boolean indicating whether the first
; string begins with the second. Be sure to follow all the steps
; of the Design Recipe for functions.

; When you are testing your function, make sure you test the case
; where the first string is shorter than the second. For example
; (string-starts-with? "fundies" "fun") should return #true but
; (string-starts-with? "fun" "fundies") should return #false.

; string-starts-with? : string string -> boolean
; checks if word starts with the letters in check and returns #true / #false
(define (string-starts-with? word check)
  (cond
    [(< (string-length word) (string-length check)) #f]
    [(string=? (substring word 0 (string-length check)) check) #t]
    [else #f]))
  