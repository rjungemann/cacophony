#lang setup/infotab

(define collection 'multi)

(define deps
  (list "osc"
        "rx"
        "scribble"))

(define build-deps
  (list "scribble-lib"
        "racket-doc"))

(define scribblings
  '(("manual.scrbl" ())))
