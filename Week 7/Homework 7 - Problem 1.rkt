;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname |Homework 7 - Problem 1|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)

; == Homework 7, Problem 1 ==

; For each TODO below, refer to the following data definition,
; and use pre-defined list abstraction(s) when appropriate.

; Note #1: part of the credit for each problem will be the choice
; of abstraction(s), so make sure they are a good match and lead
; to an effective function design!

; Note #2: just because we now have cool abstractions doesn't mean
; you should forget about the design recipe and following templates
; (which particularly come up for abstraction helpers)!


(define-struct conf [title authors year venue page-start page-end doi])
(define-struct journal [title authors year journal volume page-start page-end doi])

; A ResearchPublication (RP) is one of:
; - (make-conf String [NEList-of String] Nat String Nat Nat String)
; - (make-journal String [NEList-of String] Nat String Nat Nat Nat String)
; Interpretation: a research publication, either...
; - a conference paper's title, author(s), year of publication, conference name,
;   page start in the proceedings, page end in the proceedings,
;   digital object identifier
; - a journal article's title, author(s), year of publication, journal name,
;   journal volume, page start in the volume, page end in the volume,
;   digital object identifier

; (Note: a DOI is just a unique identifier for publications
;        https://en.wikipedia.org/wiki/Digital_object_identifier)


(define
  RP-CONF-1
  (make-conf
   (string-append
    "An MS in CS for non-CS Majors: "
    "Moving to Increase Diversity of Thought and Demographics in CS")
   (list "Carla Brodley" "Megan Barry" "Aidan Connell" "Catherine Gill"
         "Ian Gorton" "Benjamin Hescott" "Bryan Lackaye" "Cynthia LuBien"
         "Leena Razzaq" "Amit Shesh" "Tiffani Williams" "Andrea Danyluk")
   2020
   (string-append
    "SIGCSE '20: "
    "Proceedings of the 51st ACM Technical Symposium on Computer Science Education")
   1248 1254
   "10.1145/3328778.3366802"))

(define
  RP-CONF-2
  (make-conf
   (string-append
    "The Potential of Spatial Computing to Augment Memory: "
    "Investigating Recall in Virtual Memory Palaces")
   (list "Tara O’Grady" "Caglar Yildirim")
   2019
   "International Conference on Human-Computer Interaction"
   414 422
   "10.1007/978-3-030-23528-4_56"))

(define
  RP-JOURNAL-1
  (make-journal
   "Investigating sources of PII used in Facebook's targeted advertising"
   (list "Giridhari Venkatadri" "Elena Lucherini" "Piotr Sapiezyński" "Alan Mislove")
   2019
   "Proceedings on Privacy Enhancing Technologies"
   2019
   227 244
   "10.2478/popets-2019-0013"))

(define
  RP-JOURNAL-2
  (make-journal
   (string-append
    "Scalable Methods to Integrate Task Knowledge with "
    "the Three-Weight Algorithm for Hybrid Cognitive Processing via Optimization")
   (list "Nate Derbinsky" "José Bento" "Jonathan S. Yedidia")
   2014
   "Biologically Inspired Cognitive Architectures"
   8
   107 117
   "10.1016/j.bica.2014.03.007"))

(define LRP-1
  (list RP-CONF-1
        RP-CONF-2
        RP-JOURNAL-1
        RP-JOURNAL-2))



; TODO #1: design the function journal-only that accepts
; a list of research publications and produces a list of
; only the journal articles.

; journal-only : [List-of-RP] -> [List-of-RP]
; Interpretation: returns a filtered list of Research
; Publications with only journal articles
(define (journal-only LoRP)
  (filter journal? LoRP))

(check-expect (journal-only LRP-1) (list RP-JOURNAL-1
                                         RP-JOURNAL-2))

; TODO #2: design the function total-pages that accepts
; a list of research publications and counts the total
; number of pages. (Hint: counting pages when given start/
; end pairs is a bit tricky; consider that if a paper
; starts at page 1 and goes through page 3, it is actually
; 3 full pages long. For reference, the provided list of
; papers is 45 pages in total.)

; total-pages : [List-of-RP] -> Nat
; Interpretation: returns the total number of
; pages in a list of research publications

(check-expect (total-pages LRP-1) 45)

(define (total-pages LoRP)
  (foldr + 0 (map get-pages LoRP)))
              
; get-pages : RP -> Nat
; Interpretation: returns the page length

(check-expect (get-pages RP-JOURNAL-1) 18)
(check-expect (get-pages RP-CONF-1) 7)

(define (get-pages rp)
  (if (conf? rp)
      (+ 1 (- (conf-page-end rp)
              (conf-page-start rp)))
      (+ 1 (- (journal-page-end rp)
              (journal-page-start rp)))))
      

; TODO #3: design the function takes-a-village? that
; accepts a list of research publications and determines
; if all publications have at least 4 authors.

; takes-a-village? : [List-of-RP] -> Boolean
; Interpretation: returns true if all publications
; in the list have at least 4 authors

(check-expect (takes-a-village? LRP-1) #false)
(check-expect (takes-a-village? (list RP-CONF-1 RP-JOURNAL-1)) #true)

(define (takes-a-village? LoRP)
  (andmap at-least-4 (map number-of-authors LoRP)))

; number-of-authors : RP -> Nat
; Interpretation: returns the number of authors
; of a research publication

(check-expect (number-of-authors RP-JOURNAL-2) 3)
(check-expect (number-of-authors RP-CONF-2) 2)

(define (number-of-authors rp)
  (length (if (conf? rp)
              (conf-authors rp)
              (journal-authors rp))))

; at-least-4 : Nat -> Boolean
; Interpretation: returns true if there are
; at least 4 authors

(define (at-least-4 n)
  (if (<= 4 n)
      #t
      #f))

(check-expect (takes-a-village? LRP-1) #false)
(check-expect (takes-a-village? (list RP-CONF-1 RP-JOURNAL-1)) #true)


; TODO #4: design the function doi-urls that accepts a
; list of research publications and produces a corresponding
; list of URLS for those publications, produced by simply
; prefixing each with https://doi.org/ - try it in your
; browser to see each one of the papers in this assignment :)

; doi-urls : [List-of-RP] -> [List-of-Strings]
; Interpretation: produces a list of URL
; strings
(check-expect (doi-urls LRP-1) (list "https://doi.org/10.1145/3328778.3366802"
                                     "https://doi.org/10.1007/978-3-030-23528-4_56"
                                     "https://doi.org/10.2478/popets-2019-0013"
                                     "https://doi.org/10.1016/j.bica.2014.03.007"))

(define (doi-urls LoRP)
  (map add-url LoRP))

; add-url : RP -> String
; Interpretation: completes the
; https://doi.org/ URL

(check-expect (add-url RP-JOURNAL-1) "https://doi.org/10.2478/popets-2019-0013")
(check-expect (add-url RP-CONF-1) "https://doi.org/10.1145/3328778.3366802")

(define (add-url rp)
  (if (conf? rp)
      (string-append "https://doi.org/" (conf-doi rp))
      (string-append "https://doi.org/" (journal-doi rp))))

; TODO #5: design the function contains-old-research? that
; accepts a list of research publications and determines if
; any of the publications are "old", which we'll define as
; having been published before 2015 - in computing research,
; that's basically a million years ;)

; contains-old-research? : [List-of-RP] -> Boolean
; Interpretation: returns true if any of the publications
; in a list are older than 2015 and false if otherwise

(check-expect (contains-old-research? LRP-1) #t)
(check-expect (contains-old-research? (list RP-CONF-1 RP-CONF-2 RP-JOURNAL-1)) #f)

(define (contains-old-research? LoRP)
  (ormap is-old? LoRP))

; is-old? : RP -> Boolean
; Interpretation: returns true if a
; publication is older than 2015

(check-expect (is-old? RP-JOURNAL-2) #t)
(check-expect (is-old? RP-CONF-2) #f)

(define (is-old? rp)
  (cond
    [(and (conf? rp) (< (conf-year rp) 2015)) #t]
    [(and (journal? rp) (< (journal-year rp) 2015)) #t]
    [else #f]))

; TODO #6: design the function title-pyramid that accepts a
; list of research publications and produces a list of the
; publication titles, ordered by increasing length.

; title-pyramid : [List-of-RP] -> [List-of-Strings]
; Interpretation: returns a list of string titles
; ordered by increasing length

; check-expect would not be under 102 spaces

(define (title-pyramid LoRP)
  (sort (map get-title LoRP) increasing-order))

; get-title : RP -> String
; Interpretation: returns the title
; of a publication

(check-expect (get-title RP-JOURNAL-1) (string-append "Investigating sources of PII used in "
                                                      "Facebook's targeted advertising"))

(define (get-title rp)
  (if (conf? rp)
      (conf-title rp)
      (journal-title rp)))

; increasing-order : String String -> Boolean
; Interpretation: returns true if in
; assending string-length order

(check-expect (increasing-order "a" "bb") #t)
(check-expect (increasing-order "bb" "a") #f)

(define (increasing-order s1 s2)
  (if (<= (string-length s1) (string-length s2))
      #t
      #f))

; TODO #7: design the function author-list that accepts a
; list of research publications and produces a list of all
; authors (including duplicates, if there are any). The list
; should be sorted alphabetically (as the names appear; so,
; for example, "Amit Shesh" comes before "Andrea Danyluk").

; author-list : [List-of-RP] -> [List-of-Strings]
; Interpretation: returns a list of all authors
; of publications

(check-expect (author-list (list RP-CONF-2 RP-JOURNAL-2)) (list "Caglar Yildirim"
                                                                "Jonathan S. Yedidia"
                                                                "José Bento"
                                                                "Nate Derbinsky"
                                                                "Tara O’Grady"))

(define (author-list LoRP)
  (sort (foldr append-lists '() (map get-authors LoRP)) string<?))

; get-authors : RP -> [List-of-Strings]
; Interpretation: returns all authors
; in a publication

(check-expect (get-authors RP-JOURNAL-1) (list "Giridhari Venkatadri"
                                               "Elena Lucherini"
                                               "Piotr Sapiezyński"
                                               "Alan Mislove"))

(check-expect (get-authors RP-CONF-2) (list "Tara O’Grady" "Caglar Yildirim"))

(define (get-authors rp)
  (if (conf? rp)
      (conf-authors rp)
      (journal-authors rp)))

; append-lists : [List-of-Strings] [List-of-Strings] -> [List-of-Strings]
; Interpretation: inspired by Racket documentation;
; allows for the appending of multiple nested lists
; with foldr ISL function

(check-expect (append-lists (list "hello" "hi") (list "bye" "see ya")) (list "hello"
                                                                             "hi"
                                                                             "bye"
                                                                             "see ya"))

(define (append-lists f r)
  (append f r))

; TODO #8: research papers often have a page limit and it
; is important that authors remember to save space at the
; end of their paper for a bibliography (a list of all the
; prior work they refer to in the paper); you are to design
; the function citation-space to help them. This function
; accepts a number (representing how many references they
; expect they'll need) and then produces text they can
; temporarily copy into their paper as a space saver - here
; is an example if they want to save space for 2 references...

(define
  CITATION-SPACE-2
  (string-append
   "[1] author information\n"
   "title information\n"
   "venue information\n"
   "year & page information\n"
   "[2] author information\n"
   "title information\n"
   "venue information\n"
   "year & page information"))

; The [#]'s should start at 1 and increase to the number
; supplied to the function; each should have literally the
; text shown above (re: author, title, venue, and year/page
; information; and there should be a newline ("\n") between
; each line of the result (though not after the last line).

; (Note: if you'd like to see the effect of \n, just
; use the text function on the example above.)

; citation-space : Number -> String
; Interpretation: returns a string that
; can be used as a citation template

(check-expect (citation-space 2) CITATION-SPACE-2)

(define (citation-space n)
  (substring
   (apply string-append (map create-citation-template (build-list n add1)))
   0
   (- (string-length (apply string-append (map create-citation-template (build-list n add1)))) 1)))

; create-citation-template : Number -> String
; Interpretation: places the input number
; into the citation template

(check-expect (create-citation-template 1) (string-append
                                            "[1] author information\n"
                                            "title information\n"
                                            "venue information\n"
                                            "year & page information\n"))

(define (create-citation-template n)
  (string-append
   "[" (number->string n) "] author information\n"
   "title information\n"
   "venue information\n"
   "year & page information\n"))