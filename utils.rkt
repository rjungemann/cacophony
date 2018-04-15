#lang racket

(provide (all-defined-out))

(define (p . args)
  (displayln (cyan (apply format args))))

(define (cyan s) (string-append "\u001b[36m" s "\u001b[0m"))
(define (blue s) (string-append "\u001b[34m" s "\u001b[0m"))
(define (yellow s) (string-append "\u001b[33m" s "\u001b[0m"))
(define (magenta s) (string-append "\u001b[35m" s "\u001b[0m"))

(define (random-port)
  (define listener (tcp-listen 0))
  (define port #f)
  (call-with-values
    (lambda () (tcp-addresses listener #t))
    (lambda (a b c d) (set! port b)))
  (tcp-close listener)
  port)
