;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |Homework 4 - Problem 3|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)

; == Homework 4, Problem 3 ==

; Consider the following data definitions:

(define-struct video [name duration hd? genre next])

; A StreamingQueue is one of:
; - #false
; - (make-video String PosInteger Boolean Genre StreamingQueue)
; Interpretation: either an empty queue
; or a video with a name, duration in minutes,
; whether it's available in HD, and it's genre.

; StreamingQueue-temp : StreamingQueue -> ?
(define (StreamingQueue-temp sq)
  (cond
    [(boolean? sq) ...]
    [(video? sq) (... (video-name sq)
                      (video-duration sq)
                      (video-hd? sq)
                      (video-genre sq)
                      (video-next sq))]))

(define StreamingQueue-1 #false)
(define StreamingQueue-2 (make-video "Top 10 NFL Teams" 8 #true "sports" StreamingQueue-1))
(define StreamingQueue-3 (make-video "Extinct Dinosaurs" 10 #true "education" StreamingQueue-2))
(define StreamingQueue-4 (make-video "Einstein Video" 16 #false "education" StreamingQueue-3))
(define StreamingQueue-5 (make-video "Funny Laughs" 13 #true "comedy" StreamingQueue-4))
(define StreamingQueue-6 (make-video "Crazy Fight" 4 #false "action" StreamingQueue-5))

; A Name is a string
; Interpretation: name of a video

; A Duration is an integer
; Interpretation: length of a video

; A HD? is one of:
; - #true
; - #false
; Interpretation: whether video is hd

; A Genre is a string
; Interpretation: genre for a video

; A Next is one of:
; - #false
; - (make-video name duration hd? genre StreamingQueue)
; Interpretation: what should be played next

; TODO #1: complete the Design Recipe for Genre
; and StreamingQueue

; TODO #2: design the function queue-pic that produces
; an image of each title (with its duration) in the queue
; stacked vertically.

; queue-pic : StreamingQueue -> image
; Interpretation: produces an image of each title
; and duration in the queue stacked vertically
(define (queue-pic sq)
  (cond
    [(boolean? sq) (overlay (text "Last Video" 20 "black")
                            background)]
    [(video? sq) (overlay (text (video-name sq) 20 "black")
                          (overlay/xy (queue-pic (video-next sq))
                                      0 -40
                                      background))]))

(define background (square 200 "solid" "white"))

; TODO #3: design the function all-hd? that determines
; if all the videos in the queue are available in HD.

; all-hd? : StreamingQueue -> boolean
; Interpretation: returns true if all videos are
; in hd and false if otherwise
(define (all-hd? sq)
  (cond
    [(boolean? sq) #t]
    [(video? sq) (cond
                   [(not (video-hd? sq)) #f]
                   [else (all-hd? (video-next sq))])]))

(check-expect (all-hd? StreamingQueue-1) #t)
(check-expect (all-hd? StreamingQueue-2) #t)
(check-expect (all-hd? StreamingQueue-3) #t)
(check-expect (all-hd? StreamingQueue-5) #f)

; TODO #4: design the function only-short that takes a
; queue and returns a new queue with only those videos
; in the original that are at most 12 minutes.

; only-short : StreamingQueue -> StreamingQueue
; Interpretation: returns a new queue with only videos
; that are less than or equal to 12 minutes
(define (only-short sq)
  (cond
    [(boolean? sq) StreamingQueue-1]
    [(video? sq) (cond
                   [(> (video-duration sq) 12) (only-short (video-next sq))]
                   [(<= (video-duration sq) 12)
                    (make-video
                     (video-name sq) (video-duration sq) (video-hd? sq) (video-genre sq)
                     (only-short (video-next sq)))])]))

(check-expect (only-short StreamingQueue-1) StreamingQueue-1)        
(check-expect (only-short StreamingQueue-3) StreamingQueue-3)
(check-expect (only-short StreamingQueue-4) StreamingQueue-3)
(check-expect (only-short StreamingQueue-6)
              (make-video "Crazy Fight" 4 #false "action" StreamingQueue-3))

; TODO #5: design the function add-ad-time that adds 1 minute
; to the duration of every video to account for requisite ads.

; add-ad-time : StreamingQueue -> StreamingQueue
; Interpretation: adds 1 minute to the duration
(define (add-ad-time sq)
  (cond
    [(boolean? sq) StreamingQueue-1]
    [(video? sq)
     (make-video
      (video-name sq) (add1 (video-duration sq)) (video-hd? sq) (video-genre sq)
      (add-ad-time (video-next sq)))]))

(check-expect (add-ad-time StreamingQueue-3) (make-video
                                              "Extinct Dinosaurs" 11 #true "education"
                                              (make-video
                                               "Top 10 NFL Teams" 9 #true "sports" #false)))

; TODO #6: design the function any-funny? that determines if any
; video in the queue is in the comedy genre.

; any-funny? : StreamingQueue -> boolean
; Interpretation: determines if any video in queue is "comedy"
(define (any-funny? sq)
  (cond
    [(boolean? sq) #f]
    [(video? sq) (cond
                   [(string=? "comedy" (video-genre sq)) #t]
                   [else (any-funny? (video-next sq))])]))

(check-expect (any-funny? StreamingQueue-1) #f)
(check-expect (any-funny? StreamingQueue-4) #f)
(check-expect (any-funny? StreamingQueue-5) #t)
(check-expect (any-funny? StreamingQueue-6) #t)