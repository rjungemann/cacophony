#lang racket

(define current-fluid-fluidsynth
  (make-parameter "/usr/local/bin/fluidsynth"))

(define current-fluid-soundfont
  (make-parameter "/Users/rjungemann/Projects/mml.old/arachno-soundfont/Arachno SoundFont - Version 1.0.sf2"))

(struct fluid-proc [p o i e oth eth] #:mutable)

(define (thread-output-handler o)
  (thread
    (λ ()
      (let loop ()
        (define line
          (with-handlers ([exn:fail?
                           (λ (_) eof)])
            (read-line o)))
        (when (not (eof-object? line))
          (begin
            (printf "[fluidsynth] ~a\n" line)
            (loop)))))))

(define (fluid-start!)
  (define-values (p o i e)
    (apply subprocess (list #f #f #f
                            (current-fluid-fluidsynth)
                            (current-fluid-soundfont))))
  (define oth (thread-output-handler o))
  (define eth (thread-output-handler e))
  (fluid-proc p o i e oth eth))

(define (fluid-stop! f)
  (begin
    (close-output-port (fluid-proc-i f))
    (close-input-port (fluid-proc-o f))
    (close-input-port (fluid-proc-e f))
    (subprocess-kill (fluid-proc-p f) #t)
    (kill-thread (fluid-proc-oth f))
    (kill-thread (fluid-proc-eth f))))

(define (fluid-send! f msg)
  (displayln msg (fluid-proc-i f)))

(define (fluid-flush! f)
  (flush-output (fluid-proc-i f)))

#| (define f (fluid-start!)) |#
#| (fluid-send! f "noteon 1 48 64") |#
#| (fluid-flush! f) |#
#| (sleep 2) |#
#| (fluid-stop! f) |#
