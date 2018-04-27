#lang racket

(require "fluid.rkt")

(provide current-fluid-fluidsynth-path
         current-fluid-args
         current-fluid-soundfont-path
         current-fluid-proc
         fluid-start!
         fluid-stop!
         fluid-send!
         fluid-flush!)

(define current-fluid-fluidsynth-path
  (make-parameter
    (or (environment-variables-ref (current-environment-variables) #"FLUIDSYNTH_PATH")
        "/usr/local/bin/fluidsynth")))

(define current-fluid-args
  (make-parameter
    (string-split
      (bytes->string/utf-8
        (or (environment-variables-ref (current-environment-variables) #"FLUIDSYNTH_ARGS")
            #""))
      ";")))

(define current-fluid-soundfont-path
  (make-parameter
    (or (environment-variables-ref (current-environment-variables) #"FLUIDSYNTH_SOUNDFONT_PATH")
        "examples/GeneralUser GS v1.471.sf2")))

(define current-fluid-proc
  (make-parameter (box #f)))

(define (fluid-start!)
  (define f (unbox (current-fluid-proc)))
  (when (not f)
    (set-box! (current-fluid-proc)
              (fluid-full-start! (current-fluid-fluidsynth-path)
                                 (current-fluid-args)
                                 (current-fluid-soundfont-path)))))

(define (fluid-stop!)
  (define f (unbox (current-fluid-proc)))
  (when f
    (fluid-full-stop! f)
    (set-box! (current-fluid-proc) #f))
  (void))

(define (fluid-send! msg)
  (define f (unbox (current-fluid-proc)))
  (fluid-full-send! f msg))

(define (fluid-flush!)
  (define f (unbox (current-fluid-proc)))
  (fluid-full-flush! f))
