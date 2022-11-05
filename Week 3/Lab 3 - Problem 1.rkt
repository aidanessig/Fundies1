;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |Lab 3 - Problem 1|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; == Lab 3, Problem 1 ==

; Consider the following data definitions & interpretations:

(define-struct address [num st city us-state zip])

(define AddressPerm (make-address 263 "Old Dam Road" "Fairfield" "CT" 06824))
(define AddressLocal (make-address 123 "Super Funny Road" "Silly Town" "HA" 12345))

; An Address is a (make-address Nat String String String Nat)
; - where num is the number of the building on the street
; - st is the name of the street
; - city is the city the building is in
; - us-state is the state the city is in
; - and zip is the zipcode of the building
; Interpretation: a US address
 
(define-struct student [first last nuid local perm])

(define NUStudent (make-student "Aidan" "Essig" 123456789 AddressPerm AddressLocal))

(define (student-temp NUStudent)
  (...(student-first NUStudent)
      (student-last NUStudent)
      (student-nuid NUStudent)
      (student-local NUStudent)
      (student-perm NUStudent) ...))
                     
; An NUStudent is a (make-student String String PositiveNumber Address Address)
; - where first is the student's first name
; - last is the student's last name
; - nuid is the student's NUID #
; - local is the student's local address
; - and perm is the student's permanent address
; Interpretation: a Northeastern student

; TODO #1: complete the data design recipe for the
; above data definitions.

; TODO #2: Design the function student-email
; which takes an NUStudent and produces a string
; representing that student’s email address.
; For simplicity we will say that a student’s email
; address is always their last name (all lowercase),
; followed by a period, followed by the first initial
; of their first name (also lowercase), and finished
; with "@northeastern.edu".

(define (student-email NUStudent)
  (string-append (string-downcase (student-last NUStudent))
                 "."
                 (substring (string-downcase (student-first NUStudent)) 0 1)
                 "@northeastern.edu"))

; TODO #3: Design the function update-zipcode which
; takes an NUStudent and a number, representing the
; new zip code of the person and updates their permanent
; address to have that zip code. Be sure to follow the
; template!
