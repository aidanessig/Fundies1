;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |Lab 2 - Problem 2|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; == Lab 2, Problem 2 ==

; TODO: Design the function either-true? that takes two
; Boolean parameters and returns true if either (or both)
; of the parameters are true.

; You must adhere to the following restrictions:
;
; - you are only allowed to use if, the names of the
;   parameters, #true, and #false (though you may not
;   need all of these);
;
; - you are not allowed to use an if that takes
;   the following form (if parameter #true #false),
;   since this is the same as the value of parameter;
;
; - the tests for your function should cover ALL possible
;   input combinations for the parameters.
;
; And don't forget (for the rest of the class!), "designing" a function
; means to produce all 4 parts of the Design Recipe for functions!

(define (either-true? a b)
  (if a #true b))
     


  
 