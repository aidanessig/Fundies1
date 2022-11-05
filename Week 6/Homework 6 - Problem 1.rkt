;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname |Homework 6 - Problem 1|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)

; == Homework 6, Problem 1 ==

; Consider the following functions:


; good-job : [List-of NonNegReal] -> [List-of NonNegReal]
; adds 20% to all supplied costs

(check-expect
 (good-job '())
 '())

(check-expect
 (good-job
  (cons 10 (cons 5 '())))
 (cons 12 (cons 6 '())))

(define (good-job lon)
  (cond
    [(empty? lon) '()]
    [(cons? lon)
     (cons
      (add-thanks (first lon))
      (good-job (rest lon)))]))


; add-thanks : NonNegReal -> NonNegReal
; adds 20% to the supplied cost

(check-expect (add-thanks 10) 12)
(check-expect (add-thanks 5) 6)

(define (add-thanks cost)
  (* cost 1.2))


; total-length : [List-of String] -> Nat
; returns the total length of all strings in the list

(check-expect
 (total-length
  '())
 0)

(check-expect
 (total-length
  (cons "a" (cons "bb" '())))
 3)

(define (total-length los)
  (cond
    [(empty? los) 0]
    [(cons? los)
     (+
      (string-length (first los))
      (total-length (rest los)))]))



; TODO #1: abstract good-job and total-length.
; Be sure to re-define the functions using your new
; abstraction.

; abstract : [List-of Items] operator-1 operator-2 default -> ?
; Interpretation: abstraction of good-job and total-length
(define (abstract LoI o1 o2 d)
  (cond
    [(empty? LoI) d]
    [(cons? LoI)
     (o1
      (o2 (first LoI))
      (abstract (rest LoI) o1 o2 d))]))

(check-expect (abstract (list "a" "bb") + string-length 0) 3)
(check-expect (abstract '() + string-length 0) 0)
(check-expect (abstract '() cons add-thanks '()) '())
(check-expect (abstract (list 10 5) cons add-thanks '()) (list 12 6))

(define (new-good-job LoN)
  (abstract LoN cons add-thanks '()))

(check-expect (new-good-job (list 10 5)) (good-job (list 10 5)))
(check-expect (new-good-job (list 4 20)) (good-job (list 4 20)))
(check-expect (new-good-job (list 15 40)) (list 18 48))
(check-expect (new-good-job '()) (good-job '()))

(define (new-total-length LoS)
  (abstract LoS + string-length 0))

(check-expect (new-total-length (list "a" "bb")) (total-length (list "a" "bb")))
(check-expect (new-total-length (list "hello")) (total-length (list "hello")))
(check-expect (new-total-length (list "pie" "k")) 4)
(check-expect (new-total-length '()) (total-length '()))

; TODO #2: use your new function to design the function
; acronym-image, which takes in a list of strings and
; visualizes them vertically stacked, with the first
; letter bolded (to highlight the acronym). The above/align
; function will be quite useful, as will the "modern" font
; (which is fixed-width, so all letters line up nicely).
; You can assume all supplied strings will have at least
; two letters.

; acronym-image : LoS -> Image
; Interpretation: stacks strings from a list
; using the the product of (make-word string)
(define (acronym-image LoS)
  (abstract LoS above make-word (empty-scene 0 0)))

; make-word : String -> Image
; Interpretation: helper function for acronym-image
; which takes in a string and creates a text image
; with first letter bolded
(define (make-word s)
  (beside (text/font (substring s 0 1) 24 "white"
                     #f 'modern 'normal 'bold #f)
          (text/font (substring s 1 (string-length s)) 24 "white"
                     #f 'modern 'normal 'normal #f)))


(check-expect (acronym-image (list "hello" "there")) (above (make-word "hello")
                                                            (make-word "there")
                                                            (empty-scene 0 0)))

(check-expect (acronym-image (list "ha" "ha" "ha" "ha")) (above (make-word "ha")
                                                                (make-word "ha")
                                                                (make-word "ha")
                                                                (make-word "ha")
                                                                (empty-scene 0 0)))

