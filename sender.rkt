#lang racket

(require racket/udp
         osc)

(provide
  (struct-out osc-sender)
  make-osc-sender
  osc-sender-send!
  osc-sender-stop!)

(struct osc-sender (socket host port))

(define (make-osc-sender host port)
  (osc-sender (udp-open-socket) host port))

(define (osc-sender-send! sender message)
  (udp-send-to (osc-sender-socket sender)
               (osc-sender-host sender)
               (osc-sender-port sender)
               (osc-element->bytes message)))

(define (osc-sender-stop! sender)
  (udp-close (osc-sender-socket sender)))
