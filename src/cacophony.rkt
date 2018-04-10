#lang racket

(require racket/async-channel
         osc
         "receiver.rkt"
         "sender.rkt"
         "clock.rkt"
         "router.rkt"
         "logging.rkt"
         "dsl.rkt")

#| ; TODO: Doesn't work because parameters across threads. |#
#| (define (start-tcp-server!) |#
#|   (thread |#
#|     (λ () |#
#|       (define listener (tcp-listen 9871)) |#
#|       (define-values (in out) (tcp-accept listener)) |#
#|       (file-stream-buffer-mode out 'line) |#
#|       (parameterize ([current-input-port in] |#
#|                      [current-output-port out] |#
#|                      [current-error-port out]) |#
#|         (read-eval-print-loop)) |#
#|       (tcp-close listener)))) |#

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
    #| (start-tcp-server!) |#
    (start-repl)))
