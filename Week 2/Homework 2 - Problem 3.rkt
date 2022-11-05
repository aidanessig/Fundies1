;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |Homework 2 - Problem 3|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)

; == Homework 2, Problem 3 ==

; Your task is to design an interactive weekly exercise
; calendar. The program displays the current day and
; associated exercise, as allows the user to scroll forward/
; backward in the week by pressing the right/left arrow keys,
; respectively. You must do this according to the following
; sequence...


; TODO: Follow the Design Recipe for Weekday, which represents
;       the days of the week (Sunday - Saturday).


; A Weekday is one of:
; - "Monday"
; - "Friday"
; - "Sunday"
; Interpretation: represents a day of the week

(define WeekdayMon "Monday")
(define WeekdayTue "Tuesday")
(define WeekdayWed "Wednesday")
(define WeekdayThu "Thursday")
(define WeekdayFri "Friday")
(define WeekdaySat "Saturday")
(define WeekdaySun "Sunday")

(define (weekday-temp wd)
  (cond
    [(string=? wd WeekdayMon) ...]
    [(string=? wd WeekdayTue) ...]
    [(string=? wd WeekdayWed) ...]
    [(string=? wd WeekdayThu) ...]
    [(string=? wd WeekdayFri) ...]
    [(string=? wd WeekdaySat) ...]
    [(string=? wd WeekdaySun) ...]))

; An Excercise is one of:
; - "Legs"
; - "Push"
; - "Pull"
; Interpretation: represents an excercise that is
; associated with a day

(define ExcerciseLegs "Legs")
(define ExcercisePush "Push")
(define ExcercisePull "Pull")

(define (excercise-temp e)
  (cond
    [(string=? e ExcerciseLegs) ...]
    [(string=? e ExcercisePush) ...]
    [(string=? e ExcercisePull) ...]))

; TODO: Design a function exercise that returns an exercise given
;       a day of the week. Here is an example, but you are
;       free to come up with your own (as long as the activity
;       varies throughout the week, so you don't get bored):
;       - Sunday:    Climbing
;       - Monday:    Cardio
;       - Tuesday:   Upper Body + Core
;       - Wednesday: Cardio
;       - Thursday:  Lower Body + Core
;       - Friday:    Cardio
;       - Saturday:  Rest


; excercise: Weekday -> Excercise
; returns the excercise to be done on the input day

(check-expect (excercise "Monday") "Push")
(check-expect (excercise "Wednesday") "Legs")
(check-expect (excercise "Friday") "Pull")
(check-expect (excercise "Sunday") "Rest")

(define (excercise wd)
  (cond
    [(string=? WeekdayMon wd) ExcercisePush]
    [(string=? WeekdayTue wd) ExcercisePull]
    [(string=? WeekdayWed wd) ExcerciseLegs]
    [(string=? WeekdayThu wd) ExcercisePush]
    [(string=? WeekdayFri wd) ExcercisePull]
    [(string=? WeekdaySat wd) ExcerciseLegs]
    [(string=? WeekdaySun wd) "Rest"]))

  
; TODO: Design the functions next-weekday and prev-weekday.
;       The former returns the weekday after that which
;       was supplied (and Monday comes after Sunday); the
;       the latter returns the weekday before that which
;       was supplied (and Sunday comes before Monday).

; next-weekday : Weekday -> Weekday
; returns the day of week that
; comes after the supplied day

(check-expect (next-weekday "Monday") "Tuesday")
(check-expect (next-weekday "Tuesday") "Wednesday")
(check-expect (next-weekday "Friday") "Saturday")
(check-expect (next-weekday "Sunday") "Monday")

(define (next-weekday day)
  (cond
    [(string=? WeekdayMon day) WeekdayTue]
    [(string=? WeekdayTue day) WeekdayWed]
    [(string=? WeekdayWed day) WeekdayThu]
    [(string=? WeekdayThu day) WeekdayFri]
    [(string=? WeekdayFri day) WeekdaySat]
    [(string=? WeekdaySat day) WeekdaySun]
    [(string=? WeekdaySun day) WeekdayMon]))

; prev-weekday : Weekday -> Weekday
; returns the day of week that
; comes before the supplied day

(check-expect (prev-weekday "Monday") "Sunday")
(check-expect (prev-weekday "Tuesday") "Monday")
(check-expect (prev-weekday "Friday") "Thursday")
(check-expect (prev-weekday "Sunday") "Saturday")

(define (prev-weekday day)
  (cond
    [(string=? WeekdayMon day) WeekdaySun]
    [(string=? WeekdayTue day) WeekdayMon]
    [(string=? WeekdayWed day) WeekdayTue]
    [(string=? WeekdayThu day) WeekdayWed]
    [(string=? WeekdayFri day) WeekdayThu]
    [(string=? WeekdaySat day) WeekdayFri]
    [(string=? WeekdaySun day) WeekdaySat]))

; TODO: Finally, using these pieces, design the World program
;       exercise-calendar that displays the day and associated
;       exercise, as well as allowing the user to scroll forward/
;       backward in the week by pressing the right/left keys,
;       respectively. (Hint: in BSL, "right" and "left" are the
;       KeyEvent values you need to respond to; but what about
;       all the other keys?) You should supply to this function
;       the initial day of the week to show.


; excercise-calendar : Weekday -> ____
; returns a world program that allows user to scroll
; through days of week using arrow keys

(define (excercise-calendar s0)
  (big-bang s0
    [to-draw scene]
    [on-key change]))

; scene : Weekday -> Image
; returns an image containing the day
; and the corresponding workout

(define (scene s)
  (overlay (text (string-append s ": " (excercise s)) 18 "black") (empty-scene 200 200)))

; change : Weekday KeyEvent -> Weekday
; returns the previous or next day based on
; the user's arrow key input

(check-expect (change "Monday" "right") "Tuesday")
(check-expect (change "Monday" "left") "Sunday")
(check-expect (change "Wednesday" "right") "Thursday")

(define (change text key)
  (cond
    [(key=? "right" key) (next-weekday text)]
    [(key=? "left" key) (prev-weekday text)])) 