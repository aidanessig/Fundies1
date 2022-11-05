;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |Homework 4 - Problem 2|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; == Homework 4, Problem 2 ==

; Consider the following data definition:

(define-struct processing [estimate])
(define-struct shipped [estimate])

; An estimate is a string
; Interpretation: represents the date associated with an item

; estimate-temp : estimate -> ?
(define (estimate-temp e)
  (... e ...))

; A PackageStatus is one of:
; - (make-processing String)
; - (make-shipped String)
; - "on the truck"
; - #true
; Interpretation: status of a package delivery,
; either processing (i.e., not yet shipped) with
; an expected ship date, already shipped with
; an expected delivery date, on the truck for
; delivery today, or already delivered (#true)

(define PackageStatus-1 (make-processing "9/22/2020"))
(define PackageStatus-2 (make-shipped "12/20/2021"))
(define PackageStatus-3 "on the truck")
(define PackageStatus-4 #true)

; PackageStatus-temp : PackageStatus -> ?
(define (PackageStatus-temp ps)
  (cond
    [(string=? "on the truck" ps) ...]
    [(boolean=? #true ps) ...]
    [(processing? ps) (... processing-estimate ps ...)]
    [(shipped? ps) (... shipped-estimate ps ...)]))
         
; TODO #1: finish the data design recipe for PackageStatus

; TODO #2: design the function package-update,
; which given a package label (e.g., "my new ipad"),
; number of items in the shipment (e.g., 1), and
; PackageStatus, and outputs a status update.

; Some example status updates include:
; - "my new ipad (1 item) is still processing and should ship on 3/14/2021"
; - "newest HP spinoff (2 items) has shipped and should arrive on 10/19/2020"
; - "tasty cookies (4 items) is on the truck for delivery today"
; - "red stapler (1 item) has been delivered"

; Hint: if you find that the right-hand side of your cond
; branches repeat a lot of the same work, you should move
; some of that work out of the cond (possibly into helper
; functions) so your code doesn’t repeat itself.

; A label is a string
; Interpretation: represents the customer's item

; label-temp : label -> ?
(define (label-temp l)
  (cond
    [(string=? "Phone" l) ...]
    [(string=? "Refridgerator" l) ...]))

; An items is an integer
; Interpretation: represents the number of items in an order

; items-temp : items ->
(define (items-temp i)
  (... i ...))

; package-update : label items PackageStatus -> String
; Interpretation: outputs a status update relative to
; the inputs
(define (package-update label items ps)
  (cond
    [(string? ps) (create-label "t" label items)]
    [(boolean? ps) (create-label "d" label items)]
    [(processing? ps) (string-append (create-label "p" label items) (processing-estimate ps))]
    [(shipped? ps) (string-append (create-label "s" label items) (shipped-estimate ps))]))

(check-expect (package-update "iPad" 2 PackageStatus-1)
              "iPad (2 items) is still processing and should ship on 9/22/2020")
(check-expect (package-update "TV" 1 PackageStatus-2)
              "TV (1 items) has shipped and should arrive on 12/20/2021")
(check-expect (package-update "Soccer Balls" 5 PackageStatus-3)
              "Soccer Balls (5 items) is on the truck for delivery today")
(check-expect (package-update "Chips" 3 PackageStatus-4)
              "Chips (3 items) has been delivered")

; A ShipmentResult is represented by one of the following strings:
; - " items) is on the truck for delivery today"
; - " items) has been delivered"
; - " items) is still processing and should ship on "
; - " items) has shipped and should arrive on "
; Interpretation: represents the four possibilities of a shipment

; ShipmentResult-temp : ShipmentResult -> ?
(define (ShipmentResult-temp sr)
  (... sr ...))

(define ShipmentResult-truck " items) is on the truck for delivery today")
(define ShipmentResult-deliver " items) has been delivered")
(define ShipmentResult-process " items) is still processing and should ship on ")
(define ShipmentResult-ship " items) has shipped and should arrive on ")

; create-label : status label items -> String
; Interpretation: creates the package-update output
(define (create-label status label items)
  (cond
    [(string=? status "t") (string-append label " (" (number->string items) ShipmentResult-truck)]
    [(string=? status "d") (string-append label " (" (number->string items) ShipmentResult-deliver)]
    [(string=? status "p") (string-append label " (" (number->string items) ShipmentResult-process)]
    [(string=? status "s") (string-append label " (" (number->string items) ShipmentResult-ship)]))

(check-expect (create-label "t" "Pens" 5)
              "Pens (5 items) is on the truck for delivery today")
(check-expect (create-label "d" "Eggs" 12)
              "Eggs (12 items) has been delivered")
(check-expect (create-label "p" "Laptop" 1)
              "Laptop (1 items) is still processing and should ship on ")
(check-expect (create-label "s" "Paper" 250)
              "Paper (250 items) has shipped and should arrive on ")