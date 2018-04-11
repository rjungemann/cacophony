#lang racket

(require racket/udp
         osc)

(provide
  (struct-out sender)
  make-sender
  sender-send!
  sender-stop!)

(struct sender (socket host port))

(define (make-sender host port)
  (sender (udp-open-socket) host port))

(define (sender-send! sender message)
  (udp-send-to (sender-socket sender)
               (sender-host sender)
               (sender-port sender)
               (osc-element->bytes message)))

(define (sender-stop! sender)
  (udp-close (sender-socket sender)))
