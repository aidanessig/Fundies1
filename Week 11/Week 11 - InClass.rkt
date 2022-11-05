;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |Week 11 - InClass|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; exam
; - lambda
; - multiple inputs
; - trees
; - list abstraction

; graphs
; - there are no directions such as in trees
; - undireted / non-linear

; local
; - use a local if there is a struct with data
;   that needs to be accessed
; - same rule applies for helper functions

; lambda
; - if it is needed only one time


; num-pages : Wiki -> NatNumber
; Interpretation: counts the number of pages
#|(define (num-pages w)
  (local [; get-all-pages : Wiki -> [List-of-Strings]
          ; Interpretation: returns all of the pages
          (define (get-all-pages w)
            (cond
              [(empty? w) '()]
              [(cons? w)
               (append (list (page-title (first w)))
                       (page-links (first w))
                       (get-all-pages (rest w)))]))]
    (length (get-all-pages w))))|#

; merge : [List-of-Strings] [List-of-Strings] -> [List-of-Strings]
; Intepretation: combines elements into one list

(check-expect (merge (list "a" "b") (list "ab" "b" "a" "c"))
              (list "ab" "b" "a" "c"))

(check-expect (merge (list "a") (list "b" "c"))
              (list "a" "b" "c"))

(define (merge l1 l2)
            (cond
              [(empty? l1) l2]
              [(cons? l1)
               (local [(define MERGED (merge (rest l1) l2))]
                 (if (s-in-los? (first l1) MERGED)
                     MERGED
                     (cons (first l1) MERGED)))]))

; s-in-los? : String [List-of String] -> Boolean
; Interpretation: is the string in the list? 
(define (s-in-los? s los)
  (ormap
   (λ (los-s) (string=? s los-s))
   los))