;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |Exam - Problem 3|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; == Exam 1, Problem 3 ==

; Consider the following data definitions:

(define-struct helped [name waited help-length])

; A HelpedStudent (HS) is a (make-helped String NaturalNumber NaturalNumber)
; Interpretation: a helped student
; - name is the student's name
; - waited is how many minutes they waited before being helped
; - help-length is how many minutes the student was helped

(define HS-1 (make-helped "anna" 3 10))
(define HS-2 (make-helped "daniel" 12 4))


(define-struct waiting [name minutes])

; A WaitingStudent (WS) is a (make-waiting String NaturalNumber)
; Interpretation: a student waiting for help
; - name is the student's name
; - minutes is the time they've been waiting so far

(define WS-1 (make-waiting "anushka" 8))
(define WS-2 (make-waiting "damon" 3))


; A StudentQuestion is one of:
; - HelpedStudent
; - WaitingStudent
; Interpretation: a student in the help queue
; that has either been helped or is waiting


(define-struct hq [student next])

; A HelpQueue (HQ) is one of:
; - #false
; - (make-hq StudentQuestion HelpQueue)
; Interpretation: a help queue, or #false if
; it's empty

(define HQ-0 #false)
(define HQ-1 (make-hq HS-1 HQ-0))
(define HQ-2 (make-hq HS-2 HQ-1))
(define HQ-3 (make-hq WS-1 HQ-2))
(define HQ-4 (make-hq WS-2 HQ-3))
  
; TODO #1: provide examples of HelpQueue using
; each of the helped/waiting examples provided
; exactly once (but in any order you choose).


; TODO #2: design the function queue-wait that accepts
; a HelpQueue and returns the total number of minutes
; that students have waited in the queue for those that
; have not yet been helped. For example, in a queue that
; includes all the examples above, the wait would be 11,
; because anushka and damon haven't been helped (sorry!),
; so 3 + 8 = 11.

; You do not have to write templates for any of the data,
; however, we will expect that you follow the correct
; templates for these definitions (and will deduct credit
; accordingly).

; queue-wait : HelpQueue -> ?
; Interpretation: returns the total number of minutes
; that unhelped students have waited
(define (queue-wait hq)
  (cond
    [(boolean? hq) 0]
    [else (cond
            [(helped? (hq-student hq))
             (queue-wait (hq-next hq))]
            [(waiting? (hq-student hq))
             (+ (waiting-minutes (hq-student hq)) (queue-wait (hq-next hq)))])]))


(check-expect (queue-wait HQ-0) 0)
(check-expect (queue-wait HQ-1) 0)
(check-expect (queue-wait HQ-2) 0)
(check-expect (queue-wait HQ-3) 8)
(check-expect (queue-wait HQ-4) 11)
(check-expect (queue-wait (make-hq WS-1 (make-hq WS-1 #false))) 16)
(check-expect (queue-wait (make-hq WS-2 (make-hq WS-2 #false))) 6)
(check-expect (queue-wait (make-hq WS-1 #false)) 8)
(check-expect (queue-wait (make-hq WS-1 HQ-2)) 8)