#lang racket

(require racket/async-channel
         osc
         "receiver.rkt"
         "sender.rkt"
         "clock.rkt")

(define receiver (make-osc-receiver 13699))
(define sender (make-osc-sender "127.0.0.1" 13698))
(define c (make-clock 120.0 24.0))

(osc-receiver-start! receiver)
(osc-sender-start! sender)
(osc-sender-send! sender (osc-message #"/status" empty))
(clock-start! c)

; TODO: Router
(thread
  (λ ()
    (let loop ()
      (let ([message (osc-receiver-get-next-message! receiver)])
        (cond
          [message
           (printf "received value on channel ~a\n" message)]))
      (loop))))

(clock-after! c 2 4
  (λ (c t)
    (printf "clock-after! ~a ~a\n" (clock-beats c t) (clock-ppqns c t))))

(every-beat! c
  (λ (c t)
    (printf "every-beat! ~a ~a\n" (clock-beats c t) (clock-ppqns c t))))

(every-ppqn! c
  (λ (c t)
    (printf "every-ppqn! ~a ~a\n" (clock-beats c t) (clock-ppqns c t))))

(let loop ()
  (clock-tick! c)
  (loop))
