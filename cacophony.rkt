#lang racket

(require "cacophony/cacophony.rkt")

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
    (log-info "┌  ┌─┐┌─┐┌─┐┌─┐┌─┐┬ ┬┌─┐┌┐┌┬ ┬  ┐  Scheme + TSlime.vim")
    (log-info "│  │  ├─┤│  │ │├─┘├─┤│ ││││└┬┘  │  Livecoding         ")
    (log-info "└  └─┘┴ ┴└─┘└─┘┴  ┴ ┴└─┘┘└┘ ┴   ┘  Platform           ")
    (start-repl)))
