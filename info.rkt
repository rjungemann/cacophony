#lang info
(define collection "cacophony")
(define deps '("base"
               "rackunit-lib"
               "osc"
               "unix-signals"
               "threading"
               "scribble"))
(define build-deps '("scribble-lib" "racket-doc"))
(define scribblings '(("scribblings/cacophony.scrbl" ())))
(define pkg-desc "Description Here")
(define version "0.0")
(define pkg-authors '(rjungemann))
