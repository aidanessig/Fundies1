;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |Exam 2 - Problem 1|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; == Exam 2, Problem 1 ==

; TODO #1: design the function pair-then-smash that takes two lists (assumed to be the same
; length), performs one function on each pair from the two lists (the first from each list,
; then the second, ...), and then applies another function that takes the list of pairwise
; results and produces a final outcome.

; For example, consider the following parallel lists of strings and natural numbers...

(define L1 (list "a" "bb" "cccc"))
(define L2 (list 1 2 3))

; now consider the following function that determines if the length of a supplied string
; is equal to a supplied number...

(define (fp1 s n)
  (= (string-length s) n))

; applying this pairwise across L1 and L2 means (fp1 "a" 1), then (fp1 "bb" 2), ...
; and producing a result list...

(define PAIR-12 (list #true #true #false))

; so then consider the following function that asks if all the results are true...

(define (fs1 l)
  (andmap identity l))

; producing an overall answer (#false) that, in fact, one of the strings length is NOT
; equal to its corresponding number.

; For another example, consider the result of adding each pairwise element of L2
; with itself...

(define PAIR-22 (list 2 4 6))

; and then take the product of this resulting list...

(define (fs2 l)
  (foldr * 1 l))

; producing the answer of 48.
; For clarity, these examples have been supplied as the following tests;
; you can create additional tests but are not required to do so.

(check-expect
 (pair-then-smash L1 L2 fp1 fs1)
 #false)

(check-expect
 (pair-then-smash L2 L2 + fs2)
 48)

; and for good measure...

(check-expect
 (pair-then-smash '() '() fp1 fs1)
 #true)

(check-expect
 (pair-then-smash '() '() + fs2)
 1)

; Note: you may choose to implement the function with list abstractions, template(s), or a
; combination; any well-designed design can receive full credit.

; pair-then-smash : [List-of Any] [List-of Any] Function Function -> Any
; Interpretation: applies the first function to both lists and then
; applies the second to the resulting output
(define (pair-then-smash l1 l2 f1 f2)
  (local [; first-function : [List-of Any] [List-of Any] Function -> [List-of Any]
          ; Interpretation: performs the first function on the two lists
          (define (first-function l1 l2 f1)
            (cond
              [(empty? l1) '()]
              [(cons? l1) (cons
                           (f1 (first l1) (first l2))
                           (first-function (rest l1) (rest l2) f1))]))]
    (f2 (first-function l1 l2 f1))))
          

; TODO #2: using pair-then-smash, design the function palindrome? that accepts a list
; and determines if its elements are the same (via a supplied equality predicate) forward
; and backwards. Example tests have been included for clarity; you can create additional
; tests but are not required to do so. As with the previous problem, you are free to design
; your function (well) as you see fit as long as it is making effective use of the
; pair-then-smash abstraction.

; palindrome? : [List-of Any] Function -> Boolean
; Interpretation: checks if the elements in a list a
; are a palindrome based on the input function
(define (palindrome? LoA f)
  (pair-then-smash LoA (reverse LoA) f fs1))

(check-expect
 (palindrome? '() =)
 #true)

(check-expect
 (palindrome? (list 1 2) =)
 #false)

(check-expect
 (palindrome? (list 1 2 2 1) =)
 #true)

(check-expect
 (palindrome? (list 1 2 3 2 1) =)
 #true)

(check-expect
 (palindrome? (explode "level") string=?)
 #true)

(check-expect
 (palindrome? (explode "howdy!") string=?)
 #false)

(check-expect
 (palindrome?
  (explode "howdy")
  (λ (s1 s2) (boolean=? (string=? s1 "w") (string=? s2 "w"))))
 #true)