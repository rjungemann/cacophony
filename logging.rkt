#lang racket

(provide logging-start!)

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
           (printf "[~a] ~a\n" level msg)])
        (and (unbox stopper) (loop)))
      (and (unbox stopper) (loop))))
  stopper)
