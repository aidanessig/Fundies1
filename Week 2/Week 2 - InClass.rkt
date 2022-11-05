;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |Week 2 - InClass|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)

(define SKY-WIDTH 300)
(define SKY-HEIGHT 200)
(define RADIUS 25)
 
(define SUN (circle RADIUS "solid" "yellow"))
(define MOON (circle RADIUS "solid" "gray"))
(define SKY (rectangle SKY-WIDTH SKY-HEIGHT "solid" "light blue"))
(define DARKSKY (rectangle SKY-WIDTH SKY-HEIGHT "solid" "black"))
(define DIMSKY (rectangle SKY-WIDTH SKY-HEIGHT "solid" "blue"))

(define (draw-eclipse x-moon)
  (place-image MOON x-moon (/ SKY-HEIGHT 2)
   (place-image SUN (/ SKY-WIDTH 2) (/ SKY-HEIGHT 2)
                (cond
                  [(> 100 x-moon) SKY]
                  [(> 135 x-moon) DIMSKY]
                  [(> 165 x-moon) DARKSKY]
                  [(> 200 x-moon) DIMSKY]
                  [else SKY]))))


 
(animate draw-eclipse)

