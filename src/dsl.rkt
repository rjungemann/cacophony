#lang racket

(require osc
         rx/event-emitter
         "clock.rkt"
         "receiver.rkt"
         "sender.rkt")

(provide (all-defined-out))

(define current-receivers
  (make-parameter (box (list))))

(define current-clock
  (make-parameter #f))

(define current-stopper
  (make-parameter (box #f)))

(define (add-receiver port)
  (define receiver (make-osc-receiver port))
  (osc-receiver-start! receiver)
  (set-box! (current-receivers) (append (unbox (current-receivers)) (list receiver)))
  receiver)

(define (add-sender host port)
  (make-osc-sender host port))

(define (start)
  (define (tick)
    (clock-tick! (current-clock))
    (for ([receiver (unbox (current-receivers))])
      (osc-receiver-tick! receiver)))
  (set-box! (current-stopper) #t)
  (clock-start! (current-clock))
  (thread
    (λ ()
      (let loop ()
        (define clock (current-clock))
        (tick)
        (and (unbox (current-stopper)) (loop))))))

(define (stop)
  (set-box! (current-stopper) #f))

(define (clear)
  (clock-clear! (current-clock)))

(define (remove-tick-listener cb)
  (remove-listener! (clock-tick-vent (current-clock)) cb))

(define (remove-pulse-listener cb)
  (remove-listener! (clock-pulse-vent (current-clock)) cb))

(define (start-repl)
  (parameterize ([current-receivers (box (list))]
                 [current-clock (make-clock 120.0 24.0)])
    (read-eval-print-loop)))

(define (defer cb)
  (clock-at! (current-clock) (now) cb))

(define (after beats cb)
  (clock-after! (current-clock) beats cb))

(define (every beats cb)
  (clock-every! (current-clock) beats cb))

(define (<< s route . args)
  (osc-sender-send! s (osc-message route args)))

(define (>> r route cb)
  (osc-receiver-add-listener! r route cb))

(define (>* r route cb)
  (osc-receiver-remove-listener! r route cb))

(define (router-remove-listener r route cb)
  (osc-receiver-router-remove-listener! r route cb))

(define (router-remove-listeners r route)
  (osc-receiver-router-remove-listener! r route))

(define (router-clear r)
  (osc-receiver-router-clear! r))

(define (set-bpm n)
  (set-clock-bpm! (current-clock) n))

; NOTE: Broken for some reason.
(define (set-ppqn n)
  (set-clock-ppqn! (current-clock) n))

(define (beats)
  (clock-beat (current-clock)))

(define (pulses)
  (clock-pulse (current-clock)))

(define (beats->ppqn beats)
  (* (clock-ppqn (current-clock)) beats))

(define (1nd) 6)
(define (1n) 4)
(define (1nt) (/ 8.0 3.0))
(define (2nd) 3)
(define (2n) 2)
(define (2nt) (/ 4.0 3.0))
(define (4nd) 1.5)
(define (4n) 1)
(define (4nt) (/ 2.0 3.0))
(define (8nd) 0.75)
(define (8n) 0.5)
(define (8nt) (/ 1.0 3.0))
(define (16nd) 0.375)
(define (16n) 0.25)
(define (16nt) (/ 1.0 6.0))
(define (32nd) 0.1875) ; NOTE: Can't be used at 24 ppqn!
(define (32n) 0.125)
(define (32nt) (/ 1.0 12.0))
(define (64nd) 0.09375) ; NOTE: Can't be used at 24 ppqn!
(define (64n) 0.0625) ; NOTE: Can't be used at 24 ppqn!
(define (64nt) (/ 1.0 24.0))

; -------
; Helpers
; -------

(define (rotate l)
  (define n 0)
  (λ ()
    (define v (list-ref l (modulo n (length l))))
    (set! n (+ n 1))
    v))
