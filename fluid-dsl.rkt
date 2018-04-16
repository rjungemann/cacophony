#lang racket

(require "fluid.rkt")

(provide current-fluid-fluidsynth-path
         current-fluid-soundfont-path
         current-fluid-proc
         fluid-start!
         fluid-stop!
         fluid-send!
         fluid-flush!)

(define current-fluid-fluidsynth-path
  (make-parameter "/usr/local/bin/fluidsynth"))

(define current-fluid-soundfont-path
  (make-parameter "examples/GeneralUser GS v1.471.sf2"))

(define current-fluid-proc
  (make-parameter (box #f)))

(define (fluid-start!)
  (set-box! (current-fluid-proc)
            (fluid-full-start! (current-fluid-fluidsynth-path)
                               (current-fluid-soundfont-path))))

(define (fluid-stop!)
  (define f (unbox (current-fluid-proc)))
  (fluid-full-stop! f)
  (set-box! (current-fluid-proc) #f))

(define (fluid-send! msg)
  (define f (unbox (current-fluid-proc)))
  (fluid-full-send! f msg))

(define (fluid-flush!)
  (define f (unbox (current-fluid-proc)))
  (fluid-full-flush! f))
