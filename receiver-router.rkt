#lang racket

(require osc
         "receiver.rkt"
         "router.rkt")

(provide receiver-router-start!
         receiver-router-tick!)

(define (receiver-router-start! receiver router)
  (void))

(define (receiver-router-tick! receiver router)
  (define message (osc-receiver-get-next-message! receiver))
  (and message (router-trigger! router
                                (osc-message-address message)
                                (append (list (osc-message-address message))
                                        (osc-message-args message)))))
