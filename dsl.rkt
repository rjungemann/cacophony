#lang racket

(require osc
         "clock.rkt"
         "receiver.rkt"
         "sender.rkt")

(provide (all-defined-out))

(define current-receivers
  (make-parameter (box (list))))

(define current-clock
  (make-parameter #f))

(define (add-receiver port)
  (define receiver (make-osc-receiver port))
  (osc-receiver-start! receiver)
  (set-box! (current-receivers) (append (unbox (current-receivers)) (list receiver)))
  receiver)

(define (add-sender host port)
  (make-osc-sender host port))

(define (start)
  (define stopper (box #t))
  (clock-start! (current-clock))
  (thread
    (λ ()
      (let loop ()
        (clock-tick! (current-clock))
        (for ([receiver (unbox (current-receivers))])
          (osc-receiver-tick! receiver))
        (and (unbox stopper) (loop)))))
  stopper)

(define (start-repl)
  (parameterize ([current-receivers (box (list))]
                 [current-clock (make-clock 120.0 24.0)])
    (read-eval-print-loop)))

; Stop clock
; Clear clock
; Clear all listeners on route
; Clear all listeners
;
; TODO: Make sure changing bpn works reasonably
; TODO: Make sure changing ppqn works reasonably

(define (immediately cb)
  (clock-at! (current-clock) (now) cb))

(define (every pulses cb)
  (clock-every! (current-clock) pulses cb))

(define (<< s route args)
  (osc-sender-send! s (osc-message route args)))

(define (>> r route cb)
  (osc-receiver-add-listener! r route cb))

#| (define (>* r route cb) |#
#|   (osc-receiver-remove-listener! r route cb)) |#

#| (define (bpm n) |#
#|   (set-clock-bpm-ppqn! (current-clock) n (clock-ppqn (current-clock)))) |#

#| (define (ppqn n) |#
#|   (set-clock-bpm-ppqn! (current-clock) (clock-bpm (current-clock)) n)) |#

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
#| (define (32nd) (beats->ppqn 0.1875)) |#
(define (32n) (beats->ppqn 0.125))
(define (32nt) (beats->ppqn (/ 1.0 12.0)))
#| (define (64nd) (beats->ppqn 0.09375)) |#
#| (define (64n) (beats->ppqn 0.0625)) |#
(define (64nt) (beats->ppqn (/ 1.0 24.0)))

#|
  (start)
  (define r (add-receiver 13699))
  (define s (add-sender "127.0.0.1" 13698))
  (immediately (λ (e) (printf "Now starting!\n")))
  (every (+ (2n) (8n)) (λ (e) (printf "Every 2.5 beats\n")))
  (every (4n) (λ (e) (printf "Every beat\n")))
  (>> r #"/status" (λ (m) (printf "Received ~a\n" m)))
  (<< s #"/status" empty)
|#

#| (clock-set-bpm! c) |#
#| (clock-set-ppqn! c) |#
#| (clock-beat c) |#
#| (clock-pulse c) |#
#| (remove-listener! (clock-tick-vent c) cb) |#
#| (remove-listener! (clock-pulse-vent c) cb) |#

#| (define c (make-clock 120.0 24.0)) |#

#| (add-listener! |#
#|   (clock-pulse-vent c) |#
#|   (λ (e) |#
#|     (define c (clock-event-clock e)) |#
#|     (printf "triggered! ~a ~a\n" (clock-beat c) (clock-pulse c)))) |#

#| (clock-every! c 24 (λ (e) (printf "1\n"))) |#
#| (clock-every! c 60 (λ (e) (printf "2.5\n"))) |#

#| (clock-start! c) |#

#| (sleep 4.0) |#
