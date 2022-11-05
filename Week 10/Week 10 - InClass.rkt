;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |Week 10 - InClass|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(build-list 10 (lambda (a) (* a 2)))

; Trees
; - most things are in a tree
; - starts with the root
; - parent node stemming from the root
; - child node / sub tree from the parent
; - leaf comes next and has no children

; Sentinel
; - when the value is not known or not
; necessary; ending value, usually #false

; Mr. Martin: Interpret = Int-ter-pre-tate
;             String = St-rrrrrr-inggggg

; When you want to take data from a list, you need a helper function
; Every item needs its own helper function to check

; unordered list doesn't have every item
; ordered list has every item
(define-struct page [title links])

; A WebPage is a (make-page String [List-of-WebPage])
; Interpretation: the title of a web page and links to other pages

; 

(define PAGE-0 (make-page "Khoury" '()))
(define PAGE-1 (make-page "NEU" (list PAGE-0)))
(define PAGE-2 (make-page "Boston" (list PAGE-0 PAGE-1)))

