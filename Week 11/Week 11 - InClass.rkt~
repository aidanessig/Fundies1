;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |Week 9 - InClass|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; add4 : Number -> Number
; Interpretation: adds 4 to a number
(define (add4 n)
  (+ n 4))

; add4-to-all : [List-of-Numbers] -> [List-of-Numbers]
; Interpretation: adds 4 to every element
(define (add4-to-all LoN)
  (map add4 LoN))

; add3-to-all : [List-of-Numbers] -> [List-of-Numbers]
; Interpretation: adds 3 to every element
(define (add3-to-all LoN)
  (local [(define (add-3 n)
            (+ n 3))]
    (map add-3 LoN)))

; add2-to-all :
; Interpretation:
(define (add2-to-all n)
  (map (λ (n) (+ n 2)) (list 1 2 3)))

; Three Multiple Complex Inputs
; - using arguments sequentiall
; - using arguments in parallel
; - using the cross-product of the arguments


; append-lists : [List-of-Numbers] [List-of-Numbers] -> [List-of-Numbers]
; Interpretation: accepts two lists and returns ones list with all elements

(check-expect (append-lists (list 1 2 3) (list 4 5 6)) (list 1 2 3 4 5 6))
(check-expect (append-lists (list 1 2) '()) (list 1 2))
(check-expect (append-lists (list "a") (list 1 2)) (list "a" 1 2))

(define (append-lists LoN1 LoN2)
  (append LoN1 LoN2))


; append-lists-sequentiallty : [List-of-Numbers] [List-of-Numbers] -> [List-of-Numbers]
; Interpretation: accepts two lists and sequentially appends them

(check-expect (append-lists-sequentially (list 1 2 3) (list 4 5 6)) (list 1 2 3 4 5 6))
(check-expect (append-lists-sequentially (list 1 2) '()) (list 1 2))
(check-expect (append-lists-sequentially (list "a") (list 1 2)) (list "a" 1 2))

(define (append-lists-sequentially l1 l2)
  (cond
    [(empty? l1) l2]
    [(cons? l1)
     (cons
      (first l1)
      (append-lists (rest l1) l2))]))

; get-element : [List-of-Items] Number -> Item
; Interpretation: returns the element in a specified location

(check-expect (get-element (list 1 2 3) 2) 3)
(check-expect (get-element (list "a" "b" "c") 1) "b")
(check-error (get-element (list 10 3 29) 4))

(define (get-element loa nat)
  (cond
    [(empty? loa) (error "invalid")]
    [(and (cons? loa) (zero? nat)) (first loa)]
    [(and (cons? loa) (positive? nat)) (get-element (rest loa) (sub1 nat))]))

; intersect : [List-of-Numbers] [List-of-Numbers] -> [List-of-Numbers]
; Interpretation: returns all emements in the first list that occur in the second

(check-expect (get-element (list "a" "b" "c") (list "c" "d" "a") =) (list "a" "c"))
(check-expect (get-element (list 1 2 3) (list 4 5 6) string=?) '())