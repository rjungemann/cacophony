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

(define (immediately cb)
  (clock-at! (current-clock) (now) cb))

(define (every ppqn cb)
  (clock-every! (current-clock) ppqn cb))

(define (<< s route args)
  (osc-sender-send! s (osc-message route args)))

(define (>> r route cb)
  (osc-receiver-add-listener! r route cb))

(define (to-ppqn beats ppqns)
  (* (clock-ppqn (current-clock)) ppqns))

(define (4n)
  (clock-ppqn (current-clock)))

#|
  (start)
  (define r (add-receiver 13699))
  (define s (add-sender "127.0.0.1" 13698))
  (immediately (λ (c t) (printf "Now!\n")))
  (every 60.0 (λ (c t) (printf "Every 2.5 beats\n")))
  (every (4n) (λ (c t) (printf "Every beat\n")))
  (>> r #"/status" (λ (m) (printf "!!! ~a\n" m)))
  (<< s #"/status" empty)
|#
