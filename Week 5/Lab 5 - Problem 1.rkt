;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |Lab 5 - Problem 1|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)

; == Lab 5, Problem 1 ==

; Your goal is to design a slide show program. Every
; slide will have a title and a list of bullets to
; display, and each slide and item will be preceded
; by a key press.

; Consider the data definitions and examples below...

; A ListOfStrings (LoS) is one of:
; - '()
; - (cons String LoS)
; Interpretation: a list of strings

; ListOfStrings-temp : LoS -> ?
(define (ListOfStrings-temp LoS)
  (cond
    [(empty? LoS) ...]
    [(cons? LoS) (... (first LoS)
                      (ListOfStrings (rest LoS)) ...)]))

(define SLIDE-1-LOS
  (cons "Designing programs to solve problems"
        (cons "Building good habits for developing large systems with a team"
              (cons "CS: fundamental ideas, thinking" '()))))

(define SLIDE-2-LOS
  (cons "Effectively using computers as tools"
        (cons "Breaking down problems" '())))

(define SLIDE-3-LOS
  (cons "Easy to start"
        (cons "Informative feedback"
              (cons "Functional programming is a useful paradigm" '()))))


(define-struct slide [title shown hidden])

; A Slide is a (make-slide String LoS LoS)
; Interpretation: a slide's title, what bullets
; have been shown, and those that are hidden

; Slide-temp : Slide -> ?
(define (Slide-temp s)
  (... (slide-title s)
       (LoS-temp (slide-shown s))
       (LoS (slide-hidden s)) ...))

(define SLIDE-1
  (make-slide
   "What is Fundies 1 About?"
   '() SLIDE-1-LOS))

(define SLIDE-1-NEXT
  (make-slide
   "What is Fundies 1 About?"
   (cons "Designing programs to solve problems" '())
   (cons "Building good habits for developing large systems with a team"
         (cons "CS: fundamental ideas, thinking" '()))))

(define SLIDE-1-NEXT-NEXT
  (make-slide
   "What is Fundies 1 About?"
   (cons "Designing programs to solve problems"
         (cons "Building good habits for developing large systems with a team" '()))
   (cons "CS: fundamental ideas, thinking" '())))

(define SLIDE-1-DONE
  (make-slide "What is Fundies 1 About?"
              SLIDE-1-LOS '()))

(define SLIDE-2
  (make-slide "What is Computer Science?"
              '() SLIDE-2-LOS))

(define SLIDE-3
  (make-slide "Why DrRacket?"
              '() SLIDE-3-LOS))


; A Slideshow is one of:
; - '()
; - (cons Slide Slideshow)
; Interpretation: an ordered slideshow

; Slideshow-temp : Slideshow -> ?
(define (Slideshow-temp s)
  (cond
    [(empty? s) ...]
    [(cons? s) (... (first s)
                    (Slideshow-temp (rest s)) ...)]))

(define SLIDESHOW-1
  (cons SLIDE-1 (cons SLIDE-2 (cons SLIDE-3 '()))))

(define SLIDESHOW-1-NEXT
  (cons SLIDE-1-NEXT (cons SLIDE-2 (cons SLIDE-3 '()))))

(define SLIDESHOW-1-DONE
  (cons SLIDE-1-DONE (cons SLIDE-2 (cons SLIDE-3 '()))))

(define SLIDESHOW-2
  (cons SLIDE-2 (cons SLIDE-3 '())))



; TODO #1: Complete the Design Recipe for LoS, Slide
; and Slideshow by creating their templates.


; TODO #2: Design the function draw-slide that draws
; a slide, showing only its title and unhidden content
; on a large background of a fixed size. The text of
; the bullets should be arranged above each other.
; You are free to be creative about your slide design :)

(define FONT-SIZE 12)
(define FONT-COLOR "black")
(define BG (empty-scene 300 300))

; draw-slide : Slide -> Image
(define (draw-slide slide)
  (overlay
   (above
    (text/font (slide-title slide)
               FONT-SIZE FONT-COLOR #false "default" "normal" "normal" #true)
    (draw-bullets (slide-shown slide)))
   BG))

(define (draw-bullets los)
  (cond
    [(empty? los) empty-image]
    [(cons? los)
     (above
      (text (first los) FONT-SIZE FONT-COLOR)
      (draw-bullets (rest los)))]))

; TODO #3: Design the function draw-slideshow that
; draws the slideshow’s first slide; if the slideshow
; is complete, you should show "Fin" as the title to
; an empty slide. Either way, this visualization should
; be placed on a large background of fixed size.

(define FIN "Fin")
(define (FIN-SLIDE)(make-slide FIN '() '()))

; draw-slideshow : Slide -> Image
(define (draw-slideshow slideshow)
  (draw-slide
   (cond
     [(empty? slideshow) FIN-SLIDE]
     [(cons? slideshow) (first slideshow)])))

; TODO #4: Design the function advance-slide that
; moves the first entry in a slide’s hidden content
; to the end of its shown content if there is any
; hidden content. As examples, look to SLIDE-1 ->
; SLIDE-1-NEXT -> SLIDE-1-NEXT-NEXT -> SLIDE-1-DONE.
; (Hint: the append function will be quite useful.)

; advance-slide : Slide -> Slide
(define (advance-slide slide)
  (cond
    [(empty? slide) ...]
    [(cons? slide) 

; TODO #5: Design the function slide-over? that
; determines if a slide is over (none of its bullets
; are hidden).

; slide-over? : Slide -> Boolean
(define (slide-over? slide)
  (if (empty? (slide-hidden slide)) #t #f))
   
; TODO #6: Design the function advance-slideshow that
; either advances its first slide if it has more content
; to be shown or moves onto the next slide if there
; is one.


; TODO #7: Design the World program go-slideshow
; that will advance a slideshow when any key is
; pressed. The program should end (i.e., stop-when)
; when there are no more slides left, showing
; our elegant "Fin" as the last image.

