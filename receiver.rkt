#lang racket

(require racket/async-channel
         racket/udp
         osc
         "router.rkt")

(provide (struct-out receiver)
         make-receiver
         receiver-start!
         receiver-tick!
         receiver-stop!
         receiver-add-listener!
         receiver-remove-listener!
         receiver-router-remove-listener!
         receiver-router-remove-listeners!
         receiver-router-clear!)

(struct receiver (socket port buffer router)
  #:mutable)

(define (make-receiver port)
  (receiver (udp-open-socket) port (make-bytes 10000 0) (make-router)))

(define (receiver-start! r)
  (let ([socket (receiver-socket r)]
        [port (receiver-port r)])
    (udp-bind! socket "127.0.0.1" port)))

(define (receiver-tick! r)
  (let*-values ([(socket) (receiver-socket r)]
                [(port) (receiver-port r)]
                [(buffer) (receiver-buffer r)]
                [(len hostname src-port) (udp-receive!* socket buffer)])
    (and len
         (let ([message (bytes->osc-element (subbytes buffer 0 len))]
               [router (receiver-router r)])
           (router-trigger! router
                            (osc-message-address message)
                            (append (list (osc-message-address message))
                                    (osc-message-args message)))))))

(define (receiver-stop! r)
  (udp-close (receiver-socket r)))

(define (receiver-add-listener! r route cb)
  (router-add-listener! (receiver-router r) route cb))

(define (receiver-remove-listener! r route cb)
  (router-remove-listener! (receiver-router r) route cb))

(define (receiver-router-remove-listener! r route cb)
  (router-remove-listener! (receiver-router r) route cb))

(define (receiver-router-remove-listeners! r route)
  (router-remove-listeners! (receiver-router r) route))

(define (receiver-router-clear! r)
  (set-receiver-router! (receiver-router r) (make-router)))
