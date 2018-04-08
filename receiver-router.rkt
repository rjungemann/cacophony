#lang racket

(require osc
         "receiver.rkt"
         "router.rkt")

(provide receiver-router-start!)

(define (receiver-router-start! receiver router)
  (define stopper (box #t))
  (thread
    (λ ()
       (let loop ()
         (define message (osc-receiver-get-next-message! receiver))
         (and message (router-trigger! router
                                       (osc-message-address message)
                                       (append (list (osc-message-address message))
                                               (osc-message-args message))))
         (and (unbox stopper)
              (loop)))))
  stopper)
