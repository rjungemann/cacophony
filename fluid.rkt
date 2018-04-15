#lang racket

(define current-fluidsynth
  (make-parameter "/usr/local/bin/fluidsynth"))

(define current-soundfont
  (make-parameter "/Users/rjungemann/Projects/mml.old/arachno-soundfont/Arachno SoundFont - Version 1.0.sf2"))

(define-values (p o i e)
  (apply subprocess (list #f #f #f (current-fluidsynth)
                          (current-soundfont))))

(thread
  (λ ()
    (let loop ()
      (displayln (read-line o))
      (loop))))

(thread
  (λ ()
    (let loop ()
      (displayln (read-line e))
      (loop))))

(displayln "noteon 1 48 64" i)
(flush-output i)
(let loop ()
  (sleep 2))
#| (subprocess-kill p #t) |#
