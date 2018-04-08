#lang racket

(require racket/async-channel
         osc
         "receiver.rkt"
         "sender.rkt"
         "clock.rkt"
         "router.rkt"
         "receiver-router.rkt"
         "logging.rkt")

#| (now (λ () (<< #"/hello" "foo" "bar"))) |#
#| (clock-at! (current-clock) (now) |#
#|   (λ (c t) (osc-sender-send! (current-sender) (osc-message #"/hello" (list "foo" "bar"))) |#

#| (every (4n) (λ () (displayln "Tick"))) |#
#| (clock-every! (current-clock) (4n) (λ (c t) (displayln "Tick"))) |#

#| (>> #"/status" (λ (m) (printf "Received ~a\n" m))) |#

(define (stop! b)
  (set-box! b #f))

(define (basic-prompt)
  (let ([in ((current-get-interaction-input-port))])
    ((current-read-interaction) (object-name in) in)))

(define-namespace-anchor anc)
(let* ([logger (make-logger 'rosc)]
       [log-receiver (make-log-receiver logger 'info)])
  (parameterize ([current-logger logger]
                 [current-namespace (namespace-anchor->namespace anc)]
                 [current-prompt-read basic-prompt])
    (logging-start! log-receiver)

    (define receiver (make-osc-receiver 13699))
    (define sender (make-osc-sender "127.0.0.1" 13698))
    (define clock (make-clock 120.0 24.0))
    (define router (make-router))
    (define mutex (make-semaphore 1))

    (osc-receiver-start! receiver)
    (clock-start! clock)
    (printf "started-at ~a\n" (clock-started-at clock))
    (osc-sender-start! sender)
    (receiver-router-start! receiver router)

    (clock-every! clock 60.0 (λ (c t) (printf "Every 2.5 beats\n")))
    (clock-every! clock 24.0 (λ (c t) (printf "Every beat\n")))

    (thread
      (λ ()
        (let loop ()
          (clock-tick! clock)
          (osc-sender-tick! sender)
          (osc-receiver-tick! receiver)
          (receiver-router-tick! receiver router)
          (loop))))

    (osc-sender-send! sender (osc-message #"/status" empty))

    (router-add-listener! router #"/status" (λ (data) (printf "!!! ~a\n" data)))

    (read-eval-print-loop)))

; TODO: Parameterize

#| (define location (make-parameter "here")) |#
#| (displayln (location)) |#
#| (parameterize ([location "there"]) |#
#|   (displayln (location))) |#

