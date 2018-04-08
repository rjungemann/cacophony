#lang racket

(require racket/async-channel
         osc
         "receiver.rkt"
         "sender.rkt"
         "clock.rkt"
         "router.rkt"
         "logging.rkt"
         "dsl.rkt")

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
    (start-repl)))
