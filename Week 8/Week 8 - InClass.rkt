;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |Week 8 - InClass|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; mysort : [List-of Number] -> [List-of Number]
; Sorts a list of numbers 
(define (mysort lon)
  (sort lon <))

; reverse-alpha : [List-of String] -> [List-of String]
; Sorts a list of string reverse-alphabetically
(define (reverse-alpha los)
  (sort los string>?))

; len<? : String String -> Boolean
; does the first string have fewer characters?
(define (len<? s1 s2)
  (< (string-length s1)
     (string-length s2)))

; num-size : [List-of Nat] -> [List-of Nat]
; Sorts a list of numbers by number of digits
 (define (num-size lon)
  (map string->number
       (sort (map number->string lon) len<?)))

 (define (string-size lon)
  (sort lon len<?))

(define (weird x)
  ((if (even? x)
       (λ (x) 10)
       (λ (x) (+ (sqr x) 1))) x))

