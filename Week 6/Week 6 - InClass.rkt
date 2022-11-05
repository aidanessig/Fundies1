;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname |Week 6 - InClass|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; A ListOfStrings (LoS) is one of:
; - '()
; - (cons String LoS)
; Interpretation: represents a list of strings

; process-LoS : LoS ->
(define (process-LoS LoS)
  (cond
    [(empty? LoS) ...]
    [(cons? LoS) (... (first LoS)
                      (process-LoS (rest LoS)) ...)]))

(define LoS-0 '())
(define LoS-1 (cons "Alice" LoS-0))
(define LoS-2 (cons "Bob" LoS-1))