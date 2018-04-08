#lang racket

(require racket/async-channel
         racket/udp
         osc)

(provide
  (struct-out osc-sender)
  make-osc-sender
  osc-sender-start!
  osc-sender-tick!
  osc-sender-send!)

(struct osc-sender (socket channel host port))

(define (make-osc-sender host port)
  (osc-sender (udp-open-socket) (make-async-channel) host port))

(define (send! sender message)
  (udp-send-to (osc-sender-socket sender)
               (osc-sender-host sender)
               (osc-sender-port sender)
               (osc-element->bytes message)))

(define (osc-sender-start! sender)
  (void))

(define (osc-sender-tick! sender)
  (let ([message (async-channel-try-get (osc-sender-channel sender))])
    (and message (send! sender message))))

(define (osc-sender-send! sender message)
  (async-channel-put (osc-sender-channel sender) message))
