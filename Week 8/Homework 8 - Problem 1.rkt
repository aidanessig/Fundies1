;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |Homework 8 - Problem 1|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; == Homework 8, Problem 1 ==

; TODO #1: design the function my-email-style that facilitates personalizing
; messages based on a supplied opening word (such as Dear or Hi), punctuation
; used after the recipient's name (such as , or :), as well as a close for
; the message (such as -BW, which means best wishes, or Thanks!).
; For clarity, some tests have been supplied.

(check-expect
 ((my-email-style-lambda "Dear" "," "-BW") "Alice" "This is a test.")
 "Dear Alice, This is a test. -BW")

(check-expect
 ((my-email-style-lambda "Hi" ":" "Thanks!") "Alice" "This is a test.")
 "Hi Alice: This is a test. Thanks!")

(check-expect
 ((my-email-style-lambda "Yo" "," "Cheers!") "Geoff" "Your name is spelled wrong.")
 "Yo Geoff, Your name is spelled wrong. Cheers!")

(check-expect
 ((my-email-style-local "Hello" "," "Keep Practicing!") "Lebron" "MJ is better.")
 "Hello Lebron, MJ is better. Keep Practicing!")

; my-email-style-local : String String String -> [String String -> String]
; Interpretation: returns a personalized message USING LOCAL
(define (my-email-style-local o p c)
  (local [; add-message : String String -> String
          ; adds the message to email style
          (define (add-message n m)
            (string-append o " " n p " " m " " c))]
    add-message))

; my-email-style-lambda : String String String -> [String String -> String]
; Interpretation: returns a personalized message USING LAMBDA
(define (my-email-style-lambda o p c)
  (λ (n m) (string-append o " " n p " " m " " c)))