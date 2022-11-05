;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |Homework 4 - Problem 1|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; == Homework 4, Problem 1 ==

; TODO: design the data necessary to represent a book, which can
; either be physical or electronic. All books have a title, author
; and publication year. Physical books are either paperback or
; harcover, and have some number of pages. Electronic (e-books)
; have a format (pdf, epub, txt, azw, html) and a source URL.

(define-struct book-info (title author year))

; A book-info is a (make-book-info title author year)
; Interpretation: a book's key information
; - title is the title of a book
; - author is the author of a book
; - year is the publication year

; book-info-temp : book-info -> ?
(define (book-temp bi)
  (... (book-info-title bi)
       (book-info-author bi)
       (book-info-year bi) ...))

(define book-info-1 (make-book-info "Nineteen Eighty-Four" "George Orwell" 1949))
(define book-info-2 (make-book-info "The Great Gatsby" "F. Scott Fitzgerald" 1925))

; A Title is a string
; Interpretation: represents the title of a book

; title-temp : title -> ?
(define (title-temp t)
  (... t ...))

(define title-1 "The Hunt for Red October")
(define title-2 "To Kill a Mockingbird")

; An Author is a string
; Interpretation: represents an author of a book

; author-temp : author -> ?
(define (author-temp a)
  (... a ...))

(define author-1 "Tom Clancy")
(define author-2 "Harper Lee")

; A Year is an integer
; Interpretation: represents the publication year of a book

; year-temp : year -> ?
(define (year-temp y)
  (... y ...))

(define year-1 1984)
(define year-2 1960)

(define-struct physical-book (book-info style pages))

; A physical-book is a (make-physical-book book-info style pages)
; Interpretation: represnts a physical book & adds the physical aspects to a book
; - style is the physical nature of the book
; - pages is the number of pages

; A Style is represented as one of the following strings:
; - "Paperback"
; - "Hardcover"
; Interpretation: represents whether a book is paperback or hardcover

; style-temp : style -> ?
(define (style-temp s)
  (... s ...))

(define style-pb "Paperback")
(define style-hc "Hardcover")

; A Pages is an integer
; Interpretation: represents the number of pages in a book

; pages-temp : pages -> ?
(define (pages-temp p)
  (... p ...))

(define pages-1 387)
(define pages-2 281)

(define-struct electronic-book (book-info format url))

; A electronic-book is a (make-electronic-book book-info format url)
; Interpretation: represents an electornic book & adds the digital aspects to a book
; - format is digital format of a book
; - url is the source url of a book

; A Format is represented as one of the following strings:
; - "pdf"
; - "epub"
; - "txt"
; - "azw"
; - "html"
; Interpretation: represents the digital format of a book

; format-temp : format -> ?
(define (format-temp f)
  (... f ...))

(define format-pdf "PDF")
(define format-html "HTML")
(define format-epub "EPUB")

; A Url is a string
; Interpretation: represents the the source url of a book

; url-temp : url -> ?
(define (url-temp u)
  (... u ...))

(define url-1 "books.com/the-hunt-for-red-october")
(define url-2 "books.com/to-kill-a-mockingbird")

(define book-1 (make-physical-book (make-book-info title-1 author-1 year-1) style-pb pages-1))
(define book-2 (make-electronic-book (make-book-info title-2 author-2 year-2) format-epub url-2))