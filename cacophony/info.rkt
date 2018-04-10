#lang setup/infotab

(define name "Cacophony")

(define blurb '((p "Livecoding with Racket and TSlime.vim")))

(define scribblings '(("cacophony.scrbl" ())))
(define categories '(net media))
(define version "2018-04-10-22:33")
(define release-notes '((p "Initial Release!")))

;; don't compile the stuff in the berkeley subdirectory.
(define compile-omit-paths '())

;; planet-specific:
(define repositories '("4.x"))
(define required-version "5.3.0")
(define primary-file "cacophony.rkt")

#;(define homepage "http://schematics.sourceforge.net/")
#;(define url "http://schematics.sourceforge.net/")
