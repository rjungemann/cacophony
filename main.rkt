#lang racket

(require racket/async-channel
         osc
         "receiver.rkt"
         "sender.rkt"
         "clock.rkt"
         "router.rkt"
         "receiver-router.rkt"
         "logging.rkt")

#| (immediately (λ () (<< #"/hello" "foo" "bar"))) |#
#| (clock-immediately! (current-clock) |#
#|   (lambda λ (c t) |#
#|     (osc-sender-send! (current-sender) (osc-message #"/hello" (list "foo" "bar"))) |#

#| (every (4n) (λ (displayln "Tick"))) |#
#| (clock-every! (current-clock) (interval-beats (4n)) (interval-ppqns (4n)) |#
#|   (lambda λ (c t) |#
#|     (displayln "Tick"))) |#

#| (define status (ch #"/status")) |#
#| (<< status) |#

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

    (osc-receiver-start! receiver)
    (osc-sender-start! sender)
    (osc-sender-send! sender (osc-message #"/status" empty))
    (clock-start! clock)
    (receiver-router-start! receiver router)

    #| (clock-after! clock 2 4 |#
    #|   (λ (c t) |#
    #|     (printf "clock-after! ~a ~a\n" (clock-beats c t) (clock-ppqns c t)))) |#

    (clock-every-beat! clock
      (λ (c t)
        (printf "every-beat! ~a ~a\n" (clock-beats c t) (clock-ppqns c t))))

    #| (clock-every-ppqn! clock |#
    #|   (λ (c t) |#
    #|     (printf "every-ppqn! ~a ~a\n" (clock-beats c t) (clock-ppqns c t)))) |#

    (define l (router-add-listener! router #"/status"))
    (thread
      (λ ()
        (let loop ()
          (define data (listener-get! l))
          (and data (printf "!!! ~a\n" data))
          (sleep 0.25)
          (loop))))

    (read-eval-print-loop)))

; TODO: Parameterize

#| (define location (make-parameter "here")) |#
#| (displayln (location)) |#
#| (parameterize ([location "there"]) |#
#|   (displayln (location))) |#
