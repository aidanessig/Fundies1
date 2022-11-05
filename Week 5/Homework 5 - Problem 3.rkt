;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |Homework 5 - Problem 3|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)

; == Homework 5, Problem 3 ==

; Over a few homework assignments, we are going to build
; up to some interesting and impactful methods of text
; analysis - that is, using a computer to gain insights
; from data expressed in natural language as text.

; For now, you are going to design the major data types
; we will use throughout the rest of the project.
; So for each TODO below, follow the design recipe for
; data given the description and examples.

; (And don't worry if at this point the ideas don't make
; a lot of sense; for now, this is great practice with
; designing data using structures, itemizations, and
; lists.)



; TODO #1: design WordCount, which is used to represent
; how many times a particular word appears in a body
; (or "corpus") of text. For example, we might want to
; represent that the word "text" appeared 2 times in the
; phrase "this text has a lot of text I think!".
; (Note: your data should not include the phrase itself,
; only the word and count.)

; A WordCount is one of:
; - 1
; - 0
; - 5
; Interpretation: represents the number of times a
; particular word appears in a body of text

; WordCount-temp : WC -> ?
(define (WordCount-temp WC)
  (cond
    [(< WC 1) ...]
    [(> WC 1) ...]
    [(= WC 1) ...]))

; TODO #2: design CorpusWordCounts, which is a list of all
; the words in a corpus along with their counts. So, for
; example, in the sentence "hello world hello", the word
; "hello" appears 2 times and the word "world" appears 1
; time. You should make use of WordCount in your definition.
; (Note: your data should not include the corpus itself,
; only the words and their associated counts.)


; A CorpusWordCounts is one of:
; - empty
; - (cons (make-WordAndCount (string Nat)) CorpusWordCounts)

; CorpusWordCounts-temp : CWC -> ?
(define (CorpusWordCounts-temp CWC)
  (cond
    [(empty? CWC) ...]
    [(cons? CWC) (... (WordAndCount-temp (first CWC))
                      (CorpusWordCounts-temp (rest CWC)) ...)]))
                 
; TODO #3: design Polarity, which is a way of describing
; a body of text as being either positive, negative, or
; neutral. For example, the phrase "What a wonderful day!"
; might be labeled as positive, while "2020 has been a rough
; year :(" might be labeled as negative.
; (Note: your data should not include the text itself,
; only the label that can take these three values.)

; A Polarity is one of:
; - "positive"
; - "neutral"
; - "negative"
; Interpretation: represents the polarity
; of a phrase

(define POLARITY-POS "positive")
(define POLARITY-NEU "neutral")
(define POLARITY-NEG "negative")

; Polarity-temp : polarity -> ?
(define (Polarity-temp P)
  (cond
    [(string=? "positive" P) ...]
    [(string=? "neutral" P) ...]
    [(string=? "negative" P) ...]))


; TODO #4: design SentenceLabel, which is a value of -1.0, 1.0,
; or 0.0 that a reader might numerically assign a particular
; sentence. We will understand these values as corresponding to
; a Polarity value (i.e., 1.0 = positive, -1.0 = negative,
; 0.0 = neutral), but your definition just needs to represent a
; type that can only take these three numeric values.

; A SentenceLabel is one of:
; - -1.0
; - 0.0
; - 1.0
; Intepretation: associated value of a
; corresponding Polarity

(define SENTENCELABEL-POS 1.0)
(define SENTENCELABEL-NEU 0.0)
(define SENTENCELABEL-NEG -1.0)

; SentenceLabel-temp : SL -> ?
(define (SentenceLabel-temp SL)
  (cond
    [(= 1.0 SL) ...]
    [(= 0.0 SL) ...]
    [(= -1.0 SL) ...]))


; TODO #5: design WordPolarityData, which is a way of representing
; polarity data that we collect about a word in a collection of
; sentences, including the word, the number of times it occurred in
; the corpus, and the sum of a polarity score (a positive or negative
; real number) representing how positive/negative the word has
; appeared. For example, we might represent that the word "happy"
; has occurred 4 times in a body of text, and its polarity sum is 1.2.
; As another example, perhaps the word "sad" has occurred 3 times in
; a body of text, and its polarity sum is -3.6.
; (Note: your data should not include the text itself, only the word
; and its associated polarity data.)

(define-struct WordPolarityData (word WordCount PolaritySum))

; A WordPolarityData is a (make-WordPolarityData String Nat Nat)
; - word is the chosen word
; - WordCount is the number of times it appears in the corpus
; - PolaritySum is the sum ofa polarity score

; WordPolarityData-temp : WPD ->
(define (WordPolarityData-temp WPD)
  (... (WordPolarityData-word WPD)
       (WordPolarityData-WordCount WPD)
       (WordPolarityData-PolaritySum WPD) ...))
  
; TODO #6: design CorpusWordPolarityData which is a list of all
; the words in a corpus along with their polarity data. So continuing
; the last example, we'd want to have a list with both "happy"
; (along with 4 and 1.2) as well as "sad" (along with 3 and -3.6).
; You should make use of WordPolarityData in your definition.
; (Note: your data should not include the text itself, only the words
; and their associated polarity data.)

; A CorpusWordPolarityData is one of:
; - empty
; - (cons WordPolarityData CorpusWordPolarityData)
; Interpretation: list of all words in a corpus
; along with their polarity data

; CorpusWordPolarityData-temp : CWPD -> ?
(define (CorpusWordPolarityData-temp CWPD)
  (cond
    [(empty? CWPD) ...]
    [(cons? CWPD) (... (WordPolarityData-temp (first CWPD))
                       (CorpusWordPolarityData-temp (rest CWPD)) ...)]))