;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |Homework 10 - Problem 1|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; == Homework 10, Problem 1 ==

; Consider a gradebook for a class.

; In order to represent how individual assignments lead to a class
; grade, you can think that there are assignment columns (each
; with a name and total points) and calculated columns (each with
; a name, operation, and columns over which to operate).
; Operations include taking the simple average, dropping some
; number of lowest values, or weighting the columns.

; For example...
; - a total column, representing a weighted average over...
;   - a homework column (worth 30% of total), representing a simple average over...
;     - 4 assignment columns (hw1-4; each out of 20pts)
;   - a project column (out of 100pts; worth 50% of total)
;   - a quizzes column (worth 20% of total), representing a weighted average over...
;     - a pre-class quizzes column (worth 20% of quizzes; with the lowest dropped), broken into...
;       - 3 columns (pcq1-3; each out of 5pts)
;     - an in-class quizzes column (worth 80% of quizzes; with the lowest dropped), broken into...
;       - 3 columns (icq1-3; each out of 10pts)


; TODO #1: using the above description, design the data for a Gradebook.
; You should represent the gradebook description above with your examples.

(define-struct assignment [name points])

; An assignment is a (make-assignment String Number)
; Interpretation: represents a students individual
; assignment e.g. homework, project, quiz
; - name is the assignment name
; - points is the total points earned

(define (assignment-temp a)
  (... (assignment-name a)
       (assignment-points a) ...))


(define project-avg 83) ; simple number

(define hw1 (make-assignment "homework 1" 16/20))
(define hw2 (make-assignment "homework 2" 17/20))
(define hw3 (make-assignment "homework 3" 15/20))
(define hw4 (make-assignment "homework 4" 19/20))

(define hw-avg 83.75) ; simple average

(define pcq1 (make-assignment "pre-class quiz 1" 2/5))
(define pcq2 (make-assignment "pre-class quiz 2" 3/5))
(define pcq3 (make-assignment "pre-class quiz 3" 5/5))

(define pcq-avg 4/5) ; simple average of top 2

(define icq1 (make-assignment "in-class quiz 1" 10/10))
(define icq2 (make-assignment "in-class quiz 2" 6/10))
(define icq3 (make-assignment "in-class quiz 3" 8/10))

(define icq-avg 9/10) ; simple average of top 2

(define quiz-avg 88) ; weighted-avg
  
(define-struct calculated [name operation assignments weight])

; A calculated column is a (make-calculated String String [List-of-Assignments] Number)
; Interpretation: represents a column where an input
; operation can be used
; - name is the assessment's name
; - operation is one of:
;   - "simple average"
;   - "drop lowest"
;   - "weighted average"
; - assignments is the list of intended assignments
; - weight is the weight for that column

(define pc (make-calculated "pre-class" "drop lowest" (list pcq1 pcq2 pcq3) 20))
(define ic (make-calculated "in-class" "drop lowest" (list icq1 icq2 icq3) 80))

(define quizzes (make-calculated "quizzes"
                                 "weighted average"
                                 (list pc ic)
                                 20))

(define homework (make-calculated "homework"
                                  "simple average"
                                  (list hw1 hw2 hw3 hw4)
                                  30))

(define project (make-calculated "project"
                                 "simple average"
                                 (list (make-assignment "project" 83/100))
                                 50))

(define total (make-calculated "total" "weighted average" (list homework project quizzes) 100))


(define (calculated-temp c)
  (...
   (cond
     [(string=? "simple average" (calculated-operation c)) ...]
     [(string=? "drop lowest" (calculated-operation c)) ...]
     [(string=? "weighted average" (calculated-operation c)) ...])))

; TODO #2: design the function valid-gradebook, which makes sure...
; - the names of all columns aren't empty
; - the number of dropped grades is always smaller
;   than the number of columns in that calculated column
; - the weights in a weighted average make sense: there is
;   one for each column, they are all positive, and they
;   add up to 100%

; valid-gradebook : CalculatedColumn -> Boolean
; Interpretation: takes in a calculated column
; and returns true if it meets above criteria

(check-expect (valid-gradebook total) #t)

(define (valid-gradebook cc)
  (local [; empty-names : CalculatedColumn -> Boolean
          ; Interpretation: returns false if a column's
          ; name is empty and true if no empty names
          (define (empty-names cc)
            (cond
              [(empty? (calculated-assignments cc)) #t]
              [(cons? (calculated-assignments cc))
               (if (string=? "" (calculated-name (first (calculated-assignments cc))))
                   #f
                   (empty-names (make-calculated "" "" (rest (calculated-assignments cc)) "")))]))
          ; dropped-grades : CalculatedColumn-Quiz -> Boolean
          ; Interpretation: takes in a quiz and returns false
          ; if dropped grades is incorrect and true if its not
          (define (dropped-grades quiz)
            (cond
              [(empty? (calculated-assignments quiz)) #t]
              [(cons? (calculated-assignments quiz))
               (if (< (length (calculated-assignments (first (calculated-assignments quiz))))
                      (sub1 (length (calculated-assignments (first (calculated-assignments quiz))))))
                   #f
                   (dropped-grades (make-calculated "" ""
                                                    (rest (calculated-assignments quiz)) "")))]))
          ; weight-sum : CalculatedColumn -> Number
          ; Interpretation: returns the total weight
          ; of each assignment
          (define (weight-sum cc)
            (cond
              [(empty? (calculated-assignments cc)) 0]
              [(cons? (calculated-assignments cc))
               (+ (calculated-weight (first (calculated-assignments cc)))
                  (weight-sum (make-calculated "" "" (rest (calculated-assignments cc)) "")))]))]
    (if (and (empty-names cc)
             (dropped-grades (third (calculated-assignments cc)))
             (= 100 (weight-sum cc)))
        #t
        #f)))

