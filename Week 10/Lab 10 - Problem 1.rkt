;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname l10p1-starter) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)

; == Lab 10, Problem 1 ==

; Consider the following data definitions and examples...


(define-struct file [name size])

; A File is a (make-file String Nat)
; - name is the name of the file (including extension)
; - size is the size of the file in bytes

(define FILE-CV (make-file "cv.pdf" 466000))
(define FILE-HELLO (make-file "hello.rkt" 888))
(define FILE-PIC (make-file "pic.jpg" 968000))
(define FILE-SCHED (make-file "schedule.pdf" 288000))
(define FILE-P1 (make-file "p1.sql" 348))
(define FILE-P2 (make-file "p2.sql" 265))


(define-struct dir [name dirs files])

; A Directory is a (make-dir String [List-of Directory] [List-of File])
; - name is the name of the directory
; - dirs is the list of sub-directories in this directory
; - files is the list of files in this directory
;   (not including the ones in sub-directories)
 
(define DIR-EMPTY (make-dir "nada" '() '()))

(define DIR-PERSONAL (make-dir "personal"
                               (list DIR-EMPTY)
                               (list FILE-CV FILE-PIC)))

(define DIR-CS2500 (make-dir "fundies" '() (list FILE-HELLO)))
(define DIR-CS3200 (make-dir "db" '() (list FILE-P1 FILE-P2)))

(define DIR-SCHOOL (make-dir "school"
                             (list DIR-CS2500 DIR-CS3200)
                             (list FILE-SCHED)))

(define DIR-ALL (make-dir "stuff" (list DIR-PERSONAL DIR-SCHOOL) '()))



; TODO #1: write the templates for File, Directory, [List-of Directory],
; and [List-of File].



; TODO #2: design the function total-files that takes a Directory
; and produces the number of files in it, however deeply they might
; be nested inside subdirectories. For example, there are 6 files
; in the supplied DIR-ALL example.



; TODO #3: design the function file-found? that accepts a Directory and
; a string and determines if a file with that name exists in the
; directory or any of its subdirectories. For example, "hello.rkt" is a
; file that can be found in DIR-ALL.



; TODO #4: design the function rename-files that accepts a Directory
; and two Strings (src and dest), which produces a Directory with all
; the same subdirectories, but with all files named src renamed to
; dest. An example test has been supplied for clarity.

#|
(check-expect
 (rename-files DIR-ALL  "pic.jpg" "pic.jpeg")
 (make-dir "stuff"
           (list
            (make-dir "personal"
                      (list DIR-EMPTY)
                      (list
                       FILE-CV
                       (make-file "pic.jpeg" 968000)))
            DIR-SCHOOL)
           '()))
|#



; TODO #5: design the function big-files that accepts a Directory and a
; number of bytes and returns a list of file names in the directory with
; at least the supplied size, sorted alphabetically. An example test has
; been supplied for clarity.

#|
(check-expect (big-files DIR-ALL 1000)
              (list "cv.pdf" "pic.jpg" "schedule.pdf"))
|#


