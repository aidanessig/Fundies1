;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname |Week 7 - InClass|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(filter odd? (list 1 2 3 4 5))

(sort (list 3 2 1 4 5) >)

(map add1 (list 1 2 2 3 3 3))
(map list '(1 2 3) '(a b c) '(4 5 6) '(d e f))

(foldl string-append "" (list "c" "a" "t"))
(foldr string-append "" (list "c" "a" "t"))

(foldl cons empty (list "c" "a" "t"))