#lang racket

(require racket/async-channel
         racket/udp
         osc
         "router.rkt")

(provide (struct-out osc-receiver)
         make-osc-receiver
         osc-receiver-start!
         osc-receiver-tick!
         osc-receiver-add-listener!
         osc-receiver-remove-listener!
         osc-receiver-router-remove-listener!
         osc-receiver-router-remove-listeners!
         osc-receiver-router-clear!)

(struct osc-receiver (socket port buffer running? router)
  #:mutable)

(define (make-osc-receiver port)
  (osc-receiver (udp-open-socket) port (make-bytes 10000 0) #f (make-router)))

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
         (let ([message (bytes->osc-element (subbytes buffer 0 len))]
               [router (osc-receiver-router r)])
           (router-trigger! router
                            (osc-message-address message)
                            (append (list (osc-message-address message))
                                    (osc-message-args message)))))))

(define (osc-receiver-add-listener! r route cb)
  (router-add-listener! (osc-receiver-router r) route cb))

(define (osc-receiver-remove-listener! r route cb)
  (router-remove-listener! (osc-receiver-router r) route cb))

(define (osc-receiver-router-remove-listener! r route cb)
  (router-remove-listener! (osc-receiver-router r) route cb))

(define (osc-receiver-router-remove-listeners! r route)
  (router-remove-listeners! (osc-receiver-router r) route))

(define (osc-receiver-router-clear! r)
  (set-osc-receiver-router! (osc-receiver-router r) (make-router)))
