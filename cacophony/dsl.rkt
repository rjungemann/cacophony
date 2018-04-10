#lang racket

(require osc
         rx/event-emitter
         "clock.rkt"
         "receiver.rkt"
         "sender.rkt")

(provide (all-defined-out))

(define current-receivers
  (make-parameter (box (list))))

(define current-senders
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

(define (remove-receiver receiver)
  (osc-receiver-stop! receiver)
  (set-box! (current-receivers) (filter (λ (n) (equal? receiver n)) (unbox (current-receivers))))
  (void))

(define (add-sender port)
  (define sender (make-osc-sender port))
  (set-box! (current-senders) (append (unbox (current-senders)) (list sender)))
  sender)

(define (remove-sender sender)
  (osc-sender-stop! sender)
  (set-box! (current-senders) (filter (λ (n) (equal? sender n)) (unbox (current-senders))))
  (void))

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
        (and (unbox (current-stopper)) (loop)))))
  (void))

(define (stop)
  (set-box! (current-stopper) #f)
  (void))

(define (clear)
  (clock-clear! (current-clock))
  (void))

(define (remove-tick-listener cb)
  (remove-listener! (clock-tick-vent (current-clock)) cb)
  (void))

(define (remove-pulse-listener cb)
  (remove-listener! (clock-pulse-vent (current-clock)) cb)
  (void))

(define (start-repl)
  (parameterize ([current-receivers (box (list))]
                 [current-clock (make-clock 120.0 24.0)])
    (read-eval-print-loop)))

(define (defer cb)
  (clock-at! (current-clock) (now) cb))

(define (every pulses cb)
  (clock-every! (current-clock) pulses cb))

(define (<< s route args)
  (osc-sender-send! s (osc-message route args))
  (void))

(define (>> r route cb)
  (osc-receiver-add-listener! r route cb))

(define (>* r route cb)
  (osc-receiver-remove-listener! r route cb))

(define (router-remove-listener r route cb)
  (osc-receiver-router-remove-listener! r route cb)
  (void))

(define (router-remove-listeners r route)
  (osc-receiver-router-remove-listener! r route)
  (void))

(define (router-clear r)
  (osc-receiver-router-clear! r)
  (void))

(define (set-bpm n)
  (set-clock-bpm! (current-clock) n)
  (void))

; NOTE: Broken for some reason.
(define (set-ppqn n)
  (set-clock-ppqn! (current-clock) n)
  (void))

(define (beats)
  (clock-beat (current-clock)))

(define (pulses)
  (clock-pulse (current-clock)))

(define (beats->ppqn beats)
  (* (clock-ppqn (current-clock)) beats))

(define (1nd) (beats->ppqn 6))
(define (1n) (beats->ppqn 4))
(define (1nt) (beats->ppqn (/ 8.0 3.0)))
(define (2nd) (beats->ppqn 3))
(define (2n) (beats->ppqn 2))
(define (2nt) (beats->ppqn (/ 4.0 3.0)))
(define (4nd) (beats->ppqn 1.5))
(define (4n) (beats->ppqn 1))
(define (4nt) (beats->ppqn (/ 2.0 3.0)))
(define (8nd) (beats->ppqn 0.75))
(define (8n) (beats->ppqn 0.5))
(define (8nt) (beats->ppqn (/ 1.0 3.0)))
(define (16nd) (beats->ppqn 0.375))
(define (16n) (beats->ppqn 0.25))
(define (16nt) (beats->ppqn (/ 1.0 6.0)))
(define (32nd) (beats->ppqn 0.1875)) ; NOTE: Can't be used at 24 ppqn!
(define (32n) (beats->ppqn 0.125))
(define (32nt) (beats->ppqn (/ 1.0 12.0)))
(define (64nd) (beats->ppqn 0.09375)) ; NOTE: Can't be used at 24 ppqn!
(define (64n) (beats->ppqn 0.0625)) ; NOTE: Can't be used at 24 ppqn!
(define (64nt) (beats->ppqn (/ 1.0 24.0)))

(define (p . args)
  (log-info (apply format args)))

(define (status)
  (define receivers (unbox (current-receivers)))
  (define senders (unbox (current-senders)))
  (p "Receivers:")
  (for ([r receivers])
    (p "  - :~a" (osc-receiver-port r))
    (for ([(route listeners) (osc-receiver-router r)])
      (p "    - ~a (~a listeners)" route (length listeners))))
  (and (empty? receivers)
       (p "  - No receivers"))
  (p "Senders:")
  (for ([s senders])
    (p "    - ~a:~a" (osc-sender-host s) (osc-sender-port s)))
  (and (empty? senders)
       (p "  - No senders"))
  (p "Running? ~a" (unbox (current-stopper)))
  (p "BPM ~a" (clock-bpm (current-clock)))
  (p "PPQN ~a" (clock-ppqn (current-clock))))

; -------
; Helpers
; -------

(define (rotate l)
  (define n 0)
  (λ ()
    (define v (list-ref l (modulo n (length l))))
    (set! n (+ n 1))
    v))
