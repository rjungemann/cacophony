#lang racket

(require "utils.rkt")

(provide fluid-full-start!
         fluid-full-stop!
         fluid-full-send!
         fluid-full-flush!
         (struct-out fluid-proc))

(struct fluid-proc [p o i e oth eth] #:mutable)

(define (thread-output-handler o color)
  (thread
    (λ ()
      (let loop ()
        (define line
          (with-handlers ([exn:fail?
                           (λ (_) eof)])
            (read-line o)))
        (when (not (eof-object? line))
          (printf (color "[fluidsynth] ~a\n") line)
          (loop))))))

(define (fluid-full-start! fluidsynth-path fluid-args soundfont-path)
  (define-values (p o i e)
    (apply subprocess (append (list #f #f #f
                                    fluidsynth-path)
                              fluid-args
                              (list soundfont-path))))
  (define oth (thread-output-handler o cyan))
  (define eth (thread-output-handler e yellow))
  (fluid-proc p o i e oth eth))

(define (fluid-full-stop! f)
  (close-output-port (fluid-proc-i f))
  (close-input-port (fluid-proc-o f))
  (close-input-port (fluid-proc-e f))
  (subprocess-kill (fluid-proc-p f) #t)
  (kill-thread (fluid-proc-oth f))
  (kill-thread (fluid-proc-eth f)))

(define (fluid-full-send! f msg)
  (displayln msg (fluid-proc-i f)))

(define (fluid-full-flush! f)
  (flush-output (fluid-proc-i f)))
