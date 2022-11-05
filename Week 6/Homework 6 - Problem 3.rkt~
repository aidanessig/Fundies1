;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname |Homework 6 - Problem 3|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; == Homework 6, Problem 3 ==

; TODO #1: design the function item-counts that accepts a list of
; elements, a transformation function, and an equality function,
; and counts the distinct transformation results.

; The transformation function takes an element from the list and
; produces a result. The equality function takes two results of
; the transformation function and determines if they are the same.

; As a motivating example, consider counting words (supplied as
; strings): the transformation might convert each string to lower-case,
; the equality would then compare two strings to see if they are
; equal; and the resulting counts would be a list of pairings of
; distinct-lower-case words and how many times they appeared in the
; original list. SO... in case of Peter Piper
; (https://en.wikipedia.org/wiki/Peter_Piper), supplied as a list of
; strings without punctuation, you would learn that "peter" occurs 4
; times (as do "piper", "picked", "pickled", and "peppers"); "a"
; appears 3 times, and "if" appears 1 time (as does "where", "is",
; and "the").

; The function result should be a list, where each element is a
; pairing between a distinct transformation result and a count
; of how many times that result has occurred in the original list.
; You should design this data as a first step.

; You'll then design item-counts to consider each element of the
; supplied list in order. For each element, transform it and then
; add the result to the count. Adding is a bit tricky: first you
; check if that result has already been seen (and, if so, +1 to the
; associated counter); otherwise, add a new count pair of 1 to the end.

(define-struct pair-count (result count))
; A pair-count is a (make-pair-count result count)
; Interpretation: the pairing of a distinct transformation
; result and a count of how many times it occured
; - result is the transformation word
; - count is the number of times it occured

; A result is a String
; Interpretation: a distinct transformation result

; A count is a Positive Integer
; Interpretation: represents the number of occurences
; of a result word

(define PAIR-1 (make-pair-count "test" 2))
(define PAIR-2 (make-pair-count "word" 1))

(define LIST-1 (list "pig" "pig" "cow" "horse" "cat" "cow"))
(define LIST-2 (list 1 4 5 2 2 5 9))

; item-counts : [ListOfItems] Transform-Function Equality-Function -> List
(define (item-counts LoI tf ef)
  (cond
    [(empty? LoI) '()]
    [(cons? LoI) (cons
                  (make-pair-count (transformation-function-items (first LoI) tf)
                                   (num-occurences (transformation-function-lists LoI tf)
                                                   (transformation-function-items (first LoI) tf) ef))
                  (item-counts (remove-dupes (rest LoI) (first LoI) ef) tf ef))]))

(check-expect (item-counts LIST-1 string-upcase string=?) (list (make-pair-count "PIG" 2)
                                                                (make-pair-count "COW" 2)
                                                                (make-pair-count "HORSE" 1)
                                                                (make-pair-count "CAT" 1)))

(check-expect (item-counts LIST-2 add1 =) (list
                                           (make-pair-count 2 1)
                                           (make-pair-count 5 1)
                                           (make-pair-count 6 2)
                                           (make-pair-count 3 2)
                                           (make-pair-count 10 1)))
                     
; num-occurences : [ListOfItems] Item Equality-Function -> Integer
; Interpretation: returns the number of times an item
; occurs in a list
(define (num-occurences LoI i ef)
  (cond
    [(empty? LoI) 0]
    [(cons? LoI) (if (ef (first LoI) i)
                     (add1 (num-occurences (rest LoI) i ef))
                     (num-occurences (rest LoI) i ef))]))

(check-expect (num-occurences LIST-1 "pig" string=?) 2)
(check-expect (num-occurences LIST-1 "cat" string=?) 1)
(check-expect (num-occurences LIST-1 "rhinoceros" string=?) 0)
(check-expect (num-occurences LIST-2 2 =) 2)
(check-expect (num-occurences LIST-2 9 =) 1)
(check-expect (num-occurences LIST-2 8 =) 0)

; remove-dupes : [ListOfItems] Item Equality-Function -> [ListOfItems]
; Interpretation: returns a list without
; all occurences of item
(define (remove-dupes LoI s ef)
  (cond
    [(empty? LoI) '()]
    [(cons? LoI) (if (ef (first LoI) s)
                     (remove-dupes (rest LoI) s ef)
                     (cons (first LoI) (remove-dupes (rest LoI) s ef)))]))

(check-expect (remove-dupes LIST-1 "cow" string=?) (list "pig" "pig" "horse" "cat"))
(check-expect (remove-dupes LIST-2 5 =) (list 1 4 2 2 9))

; transform-function-lists : [ListOfItems] Transform-Function -> [ListOfItems]
; Interpretation: applies the transformation-function on
; all elements of a list
(define (transformation-function-lists LoI tf)
  (cond
    [(empty? LoI) '()]
    [(cons? LoI) (cons (tf (first LoI)) (transformation-function-lists (rest LoI) tf))]))

(check-expect (transformation-function-lists LIST-2 add1)
              (list 2 5 6 3 3 6 10))
(check-expect (transformation-function-lists LIST-1 string-upcase)
              (list "PIG" "PIG" "COW" "HORSE" "CAT" "COW"))

; transform-function-items : Item Transform-Function -> String
; Interpretation: applies the transformation-function on
; a select item
(define (transformation-function-items i tf)
  (tf i))

(check-expect (transformation-function-items 3 add1) 4)
(check-expect (transformation-function-items "hello" string-upcase) "HELLO")
                  