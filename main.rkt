#lang racket/base

(module+ test
  (require rackunit))

;; Notice
;; To install (from within the package directory):
;;   $ raco pkg install
;; To install (once uploaded to pkgs.racket-lang.org):
;;   $ raco pkg install <<name>>
;; To uninstall:
;;   $ raco pkg remove <<name>>
;; To view documentation:
;;   $ raco docs <<name>>
;;
;; For your convenience, we have included a LICENSE.txt file, which links to
;; the GNU Lesser General Public License.
;; If you would prefer to use a different license, replace LICENSE.txt with the
;; desired license.
;;
;; Some users like to add a `private/` directory, place auxiliary files there,
;; and require them in `main.rkt`.
;;
;; See the current version of the racket style guide here:
;; http://docs.racket-lang.org/style/index.html

(require racket/async-channel
         osc
         "receiver.rkt"
         "sender.rkt"
         "clock.rkt"
         "router.rkt"
         "logging.rkt"
         "dsl.rkt")

(provide (all-from-out "receiver.rkt"
                       "sender.rkt"
                       "clock.rkt"
                       "router.rkt"
                       "logging.rkt"
                       "dsl.rkt"))

(module+ test
  ;; Tests to be run with raco test
  )

(module+ main
  ;; Main entry point, executed when run with the `racket` executable or DrRacket.
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
      (start-repl))))
