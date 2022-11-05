;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname |Lab 6 - Problem 1|) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp")) #f)))
; == Lab 6, Problem 1 ==

; Consider the following:

; A Posn is a (make-posn Real Real)
; Interpretation: an (x, y) coordinate

; matching-x-posn : [List-of Posn] Number Posn -> Posn
; Find the first Posn in the list with the given x-coordinate
; or return the given Posn if no such position can be found

(check-expect
 (matching-x-posn
  '()
  10 (make-posn 0 0))
 (make-posn 0 0))

(check-expect
 (matching-x-posn
  (cons (make-posn 1 2) (cons (make-posn 3 4) '()))
  3 (make-posn 5 6))
 (make-posn 3 4))

(define (matching-x-posn lop desired-x default)
  (cond [(empty? lop) default]
        [(cons? lop)
         (if (= (posn-x (first lop)) desired-x)
             (first lop)
             (matching-x-posn (rest lop) desired-x default))]))


; string-with-length : [List-of String] Nat -> String
; Returns the first String in the given list with the given
; length or "no such string" if no such string can be found

(check-expect
 (string-with-length
  '()
  10)
 "no such string")

(check-expect
 (string-with-length
  (cons "hi" (cons "hello" (cons "aloha" '())))
  5)
 "hello")

(define (string-with-length los desired-length)
  (cond [(empty? los) "no such string"]
        [(cons? los)
         (if (= (string-length (first los)) desired-length)
             (first los)
             (string-with-length (rest los) desired-length))]))


; TODO #1: design the function find-first-match that abstracts
; these two functions. Be sure to redefine matching-x-posn and
; string-with-length using your abstraction.

; find-first-match : [List-of Item] Nat default operation -> ?
(define (find-first-match LoI desired-value default f)
  (cond
    [(empty? LoI) default]
    [(cons? LoI) (if (= (f (first LoI)) desired-value)
                     (first LoI)
                     (find-first-match (rest LoI) desired-value default f))]))

(check-expect
 (find-first-match
  (cons "hi" (cons "hello" (cons "aloha" '())))
  5 "no such string" string-length)
 "hello")

(check-expect
 (find-first-match
  (cons (make-posn 1 2) (cons (make-posn 3 4) '()))
  3 "position not found" posn-x)
 (make-posn 3 4))

(check-expect
 (find-first-match
  '()
  10 "position not found" posn-x)
 "position not found")

;(define (matching-x-posn LoP desired-x default)
  ;(find-first-match lop desired-x default posn-x))

;(define (string-with-length LoS desired-x)
  ;(find-first-match lop desired-x "no such string" posn-x))

; TODO #2: using find-first-match, design the function any-true?
; that returns #true if a list of Boolean data contains at least
; one #true, otherwise #false

(define (change-boolean b)
  (if b 1 0))
       
(define (any-true? LoI)
  (find-first-match LoI 1 #false change-boolean))
