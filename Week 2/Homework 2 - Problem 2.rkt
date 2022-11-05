;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |Homework 2 - Problem 2|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; == Homework 2, Problem 2 ==

; In each of the parts of this problem you will design
; Boolean functions with these restrictions:
;
; - in each part you are only allowed to use if,
;   the names of the parameters, #true, and #false
;   (though you may not need all of these);
;
; - you are not allowed to use an if that takes
;   the following form (if parameter #true #false),
;   since this is the same as the value of parameter;
;
; - the tests for your functions should cover ALL possible
;   input combinations for the parameters.
;
; And don't forget (for the rest of the class!), "designing" a function
; means to produce all 4 parts of the Design Recipe for functions!

; TODO: Design the function same? that takes two Boolean parameters
;       and returns true if either both are true or both are false.

; same? : Boolean Boolean -> Boolean
; returns true if both are true or both are false

(define (same? a b)
  (if a b (if b #f #t)))
      
(check-expect (same? #t #t) #t)
(check-expect (same? #t #f) #f)
(check-expect (same? #f #f) #t)
(check-expect (same? #f #t) #f)

; TODO: Design the function non-agreement? that takes two Boolean parameters
;       and returns true if at least one of them is false.
  
; non-agreement? : Boolean Boolean -> Boolean
; returns true if at least one is false

(define (non-agreement? a b)
  (if a (if b #f #t) #t))
  
(check-expect (non-agreement? #f #t) #t)
(check-expect (non-agreement? #t #f) #t)
(check-expect (non-agreement? #f #f) #t)
(check-expect (non-agreement? #t #t) #f)

; TODO: Design the function follow-directions that takes two Boolean parameters:
;       * if the first is false, it simply returns the second;
;       * if the first is true, it returns the opposite of the second

; follow-directions : Boolean Boolean -> Boolean
; if first is false, returns second
; if first is true, returns opposite of second

(define (follow-directions a b)
  (if a (if b #f #t) b))

(check-expect (follow-directions #f #t) #t)
(check-expect (follow-directions #t #f) #t)
(check-expect (follow-directions #f #f) #f)
(check-expect (follow-directions #t #t) #f)
