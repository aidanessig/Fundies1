;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |Homework 9 - Problem 1|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/batch-io)

; == Homework 9, Problem 1 ==

; We are going to continue working towards the project, designing several common
; functions for analyzing a text based upon the occurrence of words.

; Some parts will reference work you did in prior homework assignments - you may
; use your own solutions to those (ideally with any corrections from grading
; feedback) or the sample solutions on the course website.

; Unless otherwise specified in the problem, you should make appropriate use of
; list abstractions and local/lambda.



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Reading from Files
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


; TODO #1: design the abstraction read-from-file-with-invalid, which checks if a
; file exists (returning an empty list if it does not) and otherwise calls a
; supplied function to return a list of strings. Use this function to design
; read-words-from-file (where the list of strings is each word in the file)
; and read-lines-as-strings-from-file (where the list is each line in the file).
; Finally, design the function read-lines-as-nums-from-file, which produces
; a list of numbers, assuming every line in a file can be converted to a number.
; Some tests have been provided for clarity.


(define BAD-FILE "BADFILE.badext")

(define PETER-PIPER
  (list "peter" "piper" "picked" "a" "peck" "of" "pickled" "peppers"
        "a" "peck" "of" "pickled" "peppers" "peter" "piper" "picked"
        "if" "peter" "piper" "picked" "a" "peck" "of" "pickled"
        "peppers" "where" "is" "the" "peck" "of" "pickled" "peppers"
        "peter" "piper" "picked"))

(define SCORE-WORDS (list "happy" "sad" "panda" "movie" "work"))
(define SCORE-VALUES (list 2.2 -5 0.2 1 -1))

(check-expect
 (read-from-file-with-invalid BAD-FILE read-words)
 '())

(check-expect
 (read-words-from-file "peter.txt")
 PETER-PIPER)

(check-expect
 (read-lines-as-strings-from-file "words.txt")
 SCORE-WORDS)

(check-expect
 (read-lines-as-nums-from-file "scores.txt")
 SCORE-VALUES)

; read-from-file-with-invalid : File Function -> [List-of-Strings]
; Interpretation: performs the function on a
; fpath and returns empty list if invalid
(define (read-from-file-with-invalid fpath f)
  (if (file-exists? fpath)
      (f fpath)
      '()))

; read-words-from-file : File -> [List-of-Strings]
; Interpretation: reads the string in file using
; the read-words function
(define (read-words-from-file fpath)
  (read-from-file-with-invalid fpath read-words))

; read-lines-as-strings-from-file : File -> [List-of-Strings]
; Interpretation: reads the string in file using
; the read-lines function
(define (read-lines-as-strings-from-file fpath)
  (read-from-file-with-invalid fpath read-lines))

; read-lines-as-nums-from-file : File -> [List-of-Numbers]
; Interpretation: reads the string in the file
; by converting to numbers
(define (read-lines-as-nums-from-file fpath)
  (map string->number (read-from-file-with-invalid fpath read-lines)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; List Analysis
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


; TODO #2: design the function first-k that returns a list of the first-k
; elements of a supplied list (or as many as are in the list, if that is fewer)
; Some tests have been provided for clarity. Do NOT use ISL list abstractions.

; Hint: think of both the list and number as complex inputs.

(check-expect (first-k '() 0) '())
(check-expect (first-k '() 3) '())
(check-expect (first-k (list 1 2 3) 3) (list 1 2 3))
(check-expect (first-k (list 1 2 3 4 5) 3) (list 1 2 3))
(check-expect (first-k (list "a" "b" "c") 4) (list "a" "b" "c"))

; first-k : [List-of-Items] Nat -> [List-of-Items]
; Interpretation: returns all items up the input value
(define (first-k LoI k)
  (cond
    [(empty? LoI) '()]
    [(cons? LoI) (if (= k 0)
                     '()
                     (cons (first LoI) (first-k (rest LoI) (sub1 k))))]))

; TODO #3: design the function distinct-elements that returns the distinct
; elements that occur in a list according to an equality predicate. (Another
; way of stating this is that this function returns a new list where all the
; duplicates of the first list have been removed.) Some tests have been provided
; for clarity.

; Hints:
; - One good approach to this problem is to consider each element in the supplied
;   list and ask whether it occurs in the remainder of the list: if not, keep it;
;   otherwise don't.
; - The above approach basically works, but work some examples and you'll see that
;   the order is a bit off; to help, consider what would happen if the list were
;   analyzed in reverse order.
; - In HW8 you designed a function that will be quite useful here for finding an
;   item in a list via an equality predicate :)


(check-expect
 (distinct-elements '() =)
 '())

(check-expect
 (distinct-elements (list 1 2) =)
 (list 1 2))

(check-expect
 (distinct-elements (list 2 1 2) =)
 (list 2 1))

(check-expect
 (distinct-elements (list "a" "b" "b" "c" "b" "a" "b") string=?)
 (list "a" "b" "c"))


; distinct-elements : [List-of-Items] Function -> [List-of-Items]
; Interpretation: returns a list of items without any duplicates
(define (distinct-elements LoI f)
  (cond
    [(empty? LoI) '()]
    [(cons? LoI) (cons (first LoI)
                       (distinct-elements (filter (λ (a) (not (f (first LoI) a))) (rest LoI)) f))]))

; TODO #4: design the function words-score that accepts three lists:
; the first is a list of words (todo) to be scored, whereas the second (words)
; and third (scores) are assumed to be the same length and parallel (that is,
; the first element in the scores list is the score corresponding to the first
; word in the words list). The function should return the total score of all the
; todo words; if a todo word can't be found in the words list, its score is the
; supplied "default" score. Some tests have been provided for clarity.


(check-expect (words-score
               '()
               SCORE-WORDS SCORE-VALUES 3.14)
              0)

(check-expect (words-score
               (list "cabbage")
               SCORE-WORDS SCORE-VALUES 3.14)
              3.14)

(check-expect (words-score
               (list "what" "a" "happy" "movie")
               SCORE-WORDS SCORE-VALUES 0)
              3.2)

(check-expect (words-score
               (list "why" "so" "sad" "panda")
               SCORE-WORDS SCORE-VALUES 0)
              -4.8)

(check-expect (words-score
               (list "watching" "a" "movie" "at" "work")
               SCORE-WORDS SCORE-VALUES 0)
              0)

; words-score : [List-of-Strings] [List-of-Strings] [List-of-Numbers] -> Number
; Interpretation: returns a number representing the total score
; of todo words
(define (words-score todo words score num)
  (local [; get-score : [List-of-Strings] [List-of-Numbers] String -> Number
          ; Interpretation: returns the value of the matching string
          (define (get-score LoS LoN s)
            (if (string=? (first LoS) s)
                (first LoN)
                (get-score (rest LoS) (rest LoN) s)))]
    (cond
      [(empty? todo) 0]
      [(cons? todo) (if (ormap (λ (s) (string=? s (first todo))) words)
                        (+ (get-score words score (first todo))
                           (words-score (rest todo) words score num))
                        (if (empty? (rest todo))
                            num
                            (words-score (rest todo) words score num)))])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Frequency Analysis
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; TODO #5: design the function top-k-words that takes a list of words and
; returns the top-k words (by frequency of occurrance in the list) in
; alphabetical order. Some tests have been provided for clarity.

; This will take a few steps, most of which you've already done in this/other
; assignments; here is a suggested sequence, noting that local definitions
; are your friend :)

; 1. Count all the words (HW6)
; 2. Get the distinct counts (TODO #3)
; 3. Sort the distinct counts (biggest first)
; 4. Get the top-k of the sorted counts (TODO #2)
; 5. Get all words whose count is in the result of the previous step
; 6. Sort the resulting words alphabetically

(check-expect
 (top-k-words (list "baz" "bar" "baz" "foo") 1)
 (list "baz"))

(check-expect
 (top-k-words (list "baz" "bar" "baz" "foo") 2)
 (list "bar" "baz" "foo"))

; For reference (PETER-PIPER frequencies)...
; 4: peter piper picked peck of pickled peppers
; 3: a
; 1: if where is the

(check-expect
 (top-k-words PETER-PIPER 1)
 (list "of" "peck" "peppers" "peter" "picked" "pickled" "piper"))

(check-expect
 (top-k-words PETER-PIPER 2)
 (list "a" "of" "peck" "peppers" "peter" "picked" "pickled" "piper"))


; top-k-words : [List-of-Strings] Number -> [List-of-Strings]
; Interpretation: returns a list of strings with the top k values
(define (top-k-words LoS n)
  (local [; up-to-n : [List-of-Numbers] Number -> [List-of-Numbers]
          ; Interpretation: accepts a list of numbers
          ; and returns the top n 
          (define (up-to-n LoI n)
            (if (= n 0) '() (cons (first LoI) (first-k (rest LoI) (sub1 n)))))

          ; get-distinct-counts : [List-of-Strings] -> [List-of-Numbers]
          ; Interpretation: returns the distinct counts of strings
          ; in a list
          (define (get-distinct-counts LoS)
            (cond
              [(empty? LoS) '()]
              [(cons? LoS) (cons (length (filter (λ (a) (string=? (first LoS) a)) LoS))
                                 (get-distinct-counts (filter (λ (a) (not (string=? (first LoS) a)))
                                                              (rest LoS))))]))

          ; remove-dupes : [List-of-Items] Function -> [List-of-Items]
          ; Interpretation: removes duplicate items in a list
          (define (remove-dupes LoI f)
            (cond
              [(empty? LoI) '()]
              [(cons? LoI) (cons (first LoI)
                                 (remove-dupes (filter (λ (a) (not (f (first LoI) a)))
                                                       (rest LoI)) f))]))]
    ; main function
    (sort (remove-dupes (filter (λ (s)
                                  (>=
                                   (length
                                    (filter (λ (a) (string=? s a)) LoS))
                                   (list-ref
                                    (up-to-n
                                     (remove-dupes (sort
                                                    (get-distinct-counts
                                                     LoS) >) =)
                                     n) (sub1 n)))) LoS) string=?) string<?)))