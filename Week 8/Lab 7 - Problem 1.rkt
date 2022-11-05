;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname |Lab 7 - Problem 1|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)

; == Lab 7, Problem 1 ==

; For each TODO below, refer to the following data definitions,
; and use pre-defined list abstraction(s) when appropriate.

; Note #1: try to choose appropriate abstraction(s), finding a
; good match for an effective function design!

; Note #2: just because we now have cool abstractions doesn't mean
; you should forget about the design recipe and following templates
; (which particularly come up for abstraction helpers)!

; Note #3: feel free to create additional examples if you'd like
; (you'll probably need some for later tests anyway!)


; A Genre is one of
; - "Pop"
; - "Classical"
; - "Country"
; - "Rock"
; Interpretation: a song genre

(define GENRE-POP "Pop")
(define GENRE-CLASSICAL "Classical")
(define GENRE-COUNTRY "Country")
(define GENRE-ROCK "Rock")

(define (genre-temp g)
  (...
   (cond
     [(string=? g GENRE-POP) ...]
     [(string=? g GENRE-CLASSICAL) ...]
     [(string=? g GENRE-COUNTRY) ...]
     [(string=? g GENRE-ROCK) ...])))


(define-struct song [name artist duration genre fav?])

; A Song is a (make-song String String Nat Genre Boolean)
; Interpretation: a song
; - name is the title of the song
; - artist is the song's artist
; - duration is the length in seconds
; - genre is the song's genre
; - fav? is this a liked song?

(define SONG-1
  (make-song
   "Redesigning Women"
   "The Highwomen"
   174 GENRE-COUNTRY
   #true))

(define SONG-2
  (make-song
   "Your Song"
   "Elton John"
   241 GENRE-POP
   #true))

(define SONG-3
  (make-song
   "All Along the Watchtower"
   "Jimi Hendrix"
   241 GENRE-ROCK
   #false))

(define SONG-4
  (make-song
   "Nessun Dorma"
   "Luciano Pavarotti"
   184 GENRE-CLASSICAL
   #false))

(define (song-temp song)
  (... (song-name song) ...
       (song-artist song) ...
       (song-duration song) ...
       (genre-temp (song-genre song)) ...
       (song-fav? song) ...))

(define-struct pl [name songs])

; A Playlist is a (make-pl String [List-of Song])
; Interpretation: a sequence of songs

(define PL-0
  (make-pl "Quiet :)" '()))

(define PL-1
  (make-pl
   "Coding Beats"
   (list SONG-1 SONG-2
         SONG-3 SONG-4)))

(define (pl-temp pl)
  (... (pl-name pl) ...
       (los-temp (pl-songs pl)) ...))



; TODO #1: design the function any-pop? that determines
; if a playlist has any pop songs

; any-pop? : Playlist -> Boolean
; Interpretation: returns true if a playlist
; contains a pop song and false if otherwise
(define (any-pop? playlist)
  (ormap check-pop (pl-songs playlist)))

(check-expect (any-pop? PL-1) #t)
(check-expect (any-pop? (make-pl "test" (list SONG-1 SONG-3 SONG-4))) #f)

; check-pop : Song -> Boolean
; Interpretation: returns true if a
; song is a pop song
(define (check-pop song)
  (string=? (song-genre song) GENRE-POP))


; TODO #2: design the function all-short? that determines
; if a playlist contains only songs shorter than 5 minutes

; all-short? : Playlist -> Boolean
; Interpretation: returns true if all songs
; in a playlist are less than 5 minutes
(define (all-short? playlist)
  (andmap check-length (pl-songs playlist)))

(check-expect (all-short? PL-1) #t)

; check-length : Song -> Boolean
; Interpretation: returns true if a song
; is less than 5 minutes
(define (check-length song)
  (< (song-duration song) 300))

; TODO #3: design the function total-duration that returns
; the total length of a playlist in seconds

; total-duration : Playlist -> Nat
; Interpretation: returns the total
; length of playlist in seconds
(define (total-duration playlist)
  (foldr + 0 (map find-duration (pl-songs playlist))))

(check-expect (total-duration PL-1) 840)

; find-duration : Song -> Nat
; Interpretation: returns the
; duration of a song
(define (find-duration song)
  (song-duration song))

; TODO #4: design the function all-names that
; produces a list of all the names of all the
; songs on a playlist

; all-names : Playlist -> [ListOfNames]
; Interpretation: returns a list
; containing all the names in a Playlist
(define (all-names playlist)
  (map get-name (pl-songs playlist)))

(check-expect (all-names PL-1) (list "Redesigning Women" "Your Song" "All Along the Watchtower" "Nessun Dorma"))

; get-name : Song -> Name
; Interpretation: returns the name of a song
(define (get-name song)
  (song-name song))

; TODO #5: design the function only-faves that when
; supplied a playlist returns a new playlist (with the
; name "Faves") that only contains the liked songs.

; only-faves : Playlist -> Playlist
; Interpretation: returns a Playlist
; containing only faves
(define (only-faves playlist)
  

; TODO #6: design the World program jukebox that shows the contents
; of a playlist. Start by showing all the songs in the playlist
; (along with their duration in seconds), but when the user
; presses the "f" key the program should swap back-and-forth
; between only showing the favorites vs all the songs. Also,
; start by showing just the titles and duration; but if a person
; presses the "a" swap back-and-forth to showing the artist names
; instead. When the program ends, return the total duration of
; the supplied playlist.

; Along the way you might find that you are writing functions
; that are quite similar - these are great opportunities for
; practicing abstraction!!

; You are welcome to use other functions you designed in this lab.
; Additionally, you should apply list abstractions where appropriate.

; And since you'll need to represent not only a playlist supplied
; to jukebox, but also whether you are showing favorite songs
; or not, as well as showing name/duration vs artist, use the
; supplied additional data definition for JS.


(define-struct js [pl only-f? show-a?])

; A JukeboxState (JS) is a (make-js Playlist Boolean Boolean)
; Interpretation: the jukebox playlist as well as...
; - only-f? is showing only favorites?
; - show-a? is showing artists?

(define JS-1-F-F
  (make-js PL-1 #false #false))

(define JS-1-T-F
  (make-js PL-1 #true #false))

(define JS-1-F-T
  (make-js PL-1 #false #true))

(define JS-1-T-T
  (make-js PL-1 #true #true))

(define (js-temp js)
  (... (pl-temp (js-pl js)) ...
       (js-only-f? js) ...
       (js-show-a? js) ...))


; jukebox : Playlist -> Nat
; interactively visualizes a playlist
; and returns the total playlist duration


