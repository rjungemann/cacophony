#lang racket

(provide logging-start!)

(define (green s) (string-append "\u001b[32m" s "\u001b[0m"))
(define (cyan s) (string-append "\u001b[36m" s "\u001b[0m"))
(define (yellow s) (string-append "\u001b[33m" s "\u001b[0m"))
(define (red s) (string-append "\u001b[31m" s "\u001b[0m"))
(define (magenta s) (string-append "\u001b[35m" s "\u001b[0m"))
(define (default s) s)
(define color-for
  (hash 'debug green
        'info cyan
        'warning yellow
        'error red
        'fatal magenta
        'none default))
(define (colorize-for-level level s)
  ((hash-ref color-for level) s))

(define (logging-start! log-receiver)
  (define stopper (box #t))
  (thread
    (λ ()
      (define (loop)
        (define v (sync log-receiver))
        (define level (vector-ref v 0))
        (define msg (vector-ref v 1))
        (cond
          [(and (equal? level 'info) (regexp-match #rx"^racket/contract:" msg))
           (void)]
          [else
           (printf (colorize-for-level level "[~a] ~a\n") level msg)])
        (and (unbox stopper) (loop)))
      (and (unbox stopper) (loop))))
  stopper)
