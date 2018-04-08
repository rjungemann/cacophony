#lang racket

(require racket/async-channel
         racket/udp
         osc)

(provide
  (struct-out osc-receiver)
  make-osc-receiver
  osc-receiver-start!
  osc-receiver-tick!
  osc-receiver-stop!
  osc-receiver-get-next-message!)

(struct osc-receiver (socket channel port buffer running?)
  #:mutable)

(define (make-osc-receiver port)
  (osc-receiver (udp-open-socket) (make-async-channel) port (make-bytes 10000 0) #f))

(define (osc-receiver-start! r)
  (let ([socket (osc-receiver-socket r)]
        [port (osc-receiver-port r)])
    (udp-bind! socket "127.0.0.1" port)
    (set-osc-receiver-running?! r #t)))

(define (osc-receiver-tick! r)
  (let*-values ([(socket) (osc-receiver-socket r)]
                [(port) (osc-receiver-port r)]
                [(buffer) (osc-receiver-buffer r)]
                [(len hostname src-port) (udp-receive!* socket buffer)])
    (and len
         (let ([message (bytes->osc-element (subbytes buffer 0 len))])
           (async-channel-put (osc-receiver-channel r) message)))))

(define (osc-receiver-stop! receiver)
  (set-osc-receiver-running?! #f))

(define (osc-receiver-get-next-message! r)
  (async-channel-try-get (osc-receiver-channel r)))
