#lang racket

(require "events.rkt")

(provide
  (struct-out clock)
  (struct-out clock-event)
  now
  bpm->ms
  ppqn->ms
  make-clock
  make-clock-event
  #| clock-pulse-tick! |#
  #| clock-tick-tick! |#
  clock-tick!
  clock-start!
  clock-stop!
  clock-at!
  clock-on!
  clock-after!
  clock-next-beat!
  clock-next-measure!
  clock-every!
  clock-clear!)

(struct clock [bpm ppqn measure-length beat pulse started-at previous-at running? tick-vent pulse-vent beat-vent measure-vent] #:mutable)
(struct clock-event [clock t])

(define (now) (current-inexact-milliseconds))
(define (bpm->ms bpm) (/ 60000.0 bpm))
(define (ppqn->ms bpm ppqn) (/ (/ 60000.0 bpm) ppqn))

(define (make-clock bpm ppqn)
  (clock bpm ppqn 4 0 0 -1 -1 #f (make-event-emitter) (make-event-emitter) (make-event-emitter) (make-event-emitter)))

(define (make-clock-event c t)
  (clock-event c t))

(define (clock-measure-tick! c t)
  (trigger (clock-measure-vent c) (make-clock-event c t)))

(define (clock-beat-tick! c t)
  (define beat (clock-beat c))
  (when (= (modulo beat (clock-measure-length c)) 0)
    (clock-measure-tick! c t))
  (trigger (clock-beat-vent c) (make-clock-event c t)))

(define (clock-pulse-tick! c t)
  (define beat (clock-beat c))
  (define pulse (clock-pulse c))
  (trigger (clock-pulse-vent c) (make-clock-event c t))
  (when (= 0 pulse)
    (clock-beat-tick! c t))
  (define raw-new-pulse (+ pulse 1))
  (define new-pulse (modulo raw-new-pulse (clock-ppqn c)))
  (define new-beat (+ beat (floor (/ raw-new-pulse (clock-ppqn c)))))
  (set-clock-beat! c new-beat)
  (set-clock-pulse! c new-pulse))

(define (clock-tick-tick! c t)
  (trigger (clock-tick-vent c) (make-clock-event c t)))

(define (clock-tick! c)
  (define t (now))
  (clock-tick-tick! c t)
  (define since (- t (clock-previous-at c)))
  (define every-ms (ppqn->ms (clock-bpm c) (clock-ppqn c)))
  (when (<= every-ms since)
    (clock-pulse-tick! c t)
    (set-clock-previous-at! c t)))

(define (clock-start! c)
  (set-clock-started-at! c (now))
  (set-clock-previous-at! c (now))
  (set-clock-running?! c #t))

(define (clock-stop! c)
  (set-clock-running?! c #f))

(define (clock-at! c t cb)
  (define (listener e)
    (when (<= (clock-event-t e) t)
      (cb e)
      (remove-listener! (clock-tick-vent (clock-event-clock e)) listener)))
  (add-listener! (clock-tick-vent c) listener))

(define (clock-on! c beats cb)
  (define (new-cb e)
    (define c (clock-event-clock e))
    (define ppqn (clock-ppqn c))
    (define current-beat (clock-beat c))
    (define current-pulse (clock-pulse c))
    (define current-beats (+ current-beat (/ current-pulse ppqn)))
    (when (<= beats current-beats)
      (cb e)
      (remove-listener! (clock-pulse-vent c) new-cb)))
  (add-listener! (clock-pulse-vent c) new-cb)
  new-cb)

(define (clock-after! c beats cb)
  (define ppqn (clock-ppqn c))
  (define current-beat (clock-beat c))
  (define current-pulse (clock-pulse c))
  (define current-beats (+ current-beat (/ current-pulse ppqn)))
  (clock-on! c (+ beats current-beats) cb))

(define (clock-next-beat! c cb)
  (define (listener e)
    (cb e)
    (remove-listener! (clock-beat-vent (clock-event-clock e)) listener))
  (add-listener! (clock-beat-vent c) listener))

(define (clock-next-measure! c cb)
  (define (listener e)
    (cb e)
    (remove-listener! (clock-measure-vent (clock-event-clock e)) listener))
  (add-listener! (clock-measure-vent c) listener))

(define (clock-every! c beats cb)
  (define previous-beat (clock-beat c))
  (define previous-pulse (clock-pulse c))
  (define (new-cb e)
    (define bpm (clock-beat c))
    (define ppqn (clock-ppqn c))
    (define current-pulses (+ (* bpm ppqn) (clock-pulse c)))
    (define previous-pulses (+ (* previous-beat ppqn) previous-pulse))
    (define pulses (* beats ppqn))
    (define beat (floor (/ pulses ppqn)))
    (define pulse (modulo pulses ppqn))
    (define pulse-offset (+ (* beat ppqn) pulse))
    (when (<= (+ previous-pulses pulse-offset) current-pulses)
      (cb e)
      (set! previous-beat bpm)
      (set! previous-pulse (clock-pulse c))))
  (add-listener! (clock-pulse-vent c) new-cb)
  new-cb)

(define (clock-clear! c)
  (clear-listeners! (clock-tick-vent c))
  (clear-listeners! (clock-pulse-vent c)))
