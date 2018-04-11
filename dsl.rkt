#lang racket

(require osc
         rx/event-emitter
         "clock.rkt"
         "receiver.rkt"
         "sender.rkt"
         "socket.rkt")

(provide (all-defined-out))

(define current-receivers
  (make-parameter (box (list))))

(define current-senders
  (make-parameter (box (list))))

(define current-clock
  (make-parameter #f))

(define current-stopper
  (make-parameter (box #f)))

(define current-ich
  (make-parameter (make-channel)))

(define current-och
  (make-parameter (make-channel)))

(define socket-port
  (make-parameter 1234))

(define (add-receiver port)
  (define receiver (make-receiver port))
  (receiver-start! receiver)
  (set-box! (current-receivers) (append (unbox (current-receivers)) (list receiver)))
  receiver)

(define (remove-receiver receiver)
  (receiver-stop! receiver)
  (set-box! (current-receivers) (filter (λ (n) (equal? receiver n)) (unbox (current-receivers))))
  (void))

(define (add-sender host port)
  (define sender (make-sender host port))
  (set-box! (current-senders) (append (unbox (current-senders)) (list sender)))
  sender)

(define (remove-sender sender)
  (sender-stop! sender)
  (set-box! (current-senders) (filter (λ (n) (equal? sender n)) (unbox (current-senders))))
  (void))

(define (start)
  (define (tick)
    (clock-tick! (current-clock))
    (socket-eval! (current-ich) (current-och))
    (for ([receiver (unbox (current-receivers))])
      (receiver-tick! receiver)))
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

(define (splash)
  (p "┌  ┌─┐┌─┐┌─┐┌─┐┌─┐┬ ┬┌─┐┌┐┌┬ ┬  ┐  Scheme + TSlime.vim")
  (p "│  │  ├─┤│  │ │├─┘├─┤│ ││││└┬┘  │  Livecoding         ")
  (p "└  └─┘┴ ┴└─┘└─┘┴  ┴ ┴└─┘┘└┘ ┴   ┘  Platform           "))

(define (start-repl anc)
  (parameterize ([current-namespace (namespace-anchor->namespace anc)]
                 [current-prompt-read (λ ()
                                        (let ([in ((current-get-interaction-input-port))])
                                          ((current-read-interaction) (object-name in) in)))]
                 [current-receivers (box (list))]
                 [current-clock (make-clock 120.0 24.0)])
    (let ([ich (current-ich)]
          [och (current-och)]
          [port (socket-port)])
      (thread
        (lambda ()
          (socket-start! port ich och))))

    (splash)
    (read-eval-print-loop)))

(define (defer cb)
  (clock-at! (current-clock) (now) cb))

(define (after beats cb)
  (clock-after! (current-clock) beats cb))

(define (every beats cb)
  (clock-every! (current-clock) beats cb))

(define (<< s route . args)
  (sender-send! s (osc-message route args))
  (void))

(define (>> r route cb)
  (receiver-add-listener! r route cb))

(define (>* r route cb)
  (receiver-remove-listener! r route cb))

(define (router-remove-listener r route cb)
  (receiver-router-remove-listener! r route cb)
  (void))

(define (router-remove-listeners r route)
  (receiver-router-remove-listener! r route)
  (void))

(define (router-clear r)
  (receiver-router-clear! r)
  (void))

(define (set-bpm n)
  (set-clock-bpm! (current-clock) n)
  (void))

(define (set-ppqn n)
  (set-clock-ppqn! (current-clock) n)
  (void))

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

(define (cyan s) (string-append "\u001b[36m" s "\u001b[0m"))

(define (p . args)
  (displayln (cyan (apply format args))))

(define (status)
  (define receivers (unbox (current-receivers)))
  (define senders (unbox (current-senders)))
  (p "Receivers:")
  (for ([r receivers])
    (p "  - :~a" (receiver-port r))
    (for ([(route listeners) (receiver-router r)])
      (p "    - ~a (~a listeners)" route (length listeners))))
  (and (empty? receivers)
       (p "  - No receivers"))
  (p "Senders:")
  (for ([s senders])
    (p "    - ~a:~a" (sender-host s) (sender-port s)))
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

(define (lerp v w t)
  (cond [(< t 0.0) v]
        [(> t 1.0) w]
        [else (+ (* (- 1 t) v) (* t w))]))

(define (lerper v w incr)
  (define t 0)
  (λ ()
    (define v (lerp v w t))
    (set! t (+ incr t))
    v))
