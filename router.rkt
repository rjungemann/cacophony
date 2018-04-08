#lang racket

(require racket/async-channel)

(provide make-router
         router-add-listener!
         router-remove-listener!
         router-trigger!
         listener-get!)

(define (make-router)
  (make-hash))

(define (router-listeners r route)
  (hash-ref r route list))

(define (router-add-listener! r route)
  (define ch (make-async-channel))
  (hash-set! r route (append (router-listeners r route) (list ch)))
  ch)

(define (router-remove-listener! r route ch)
  (hash-set! r route (filter (λ (n) (equal? n ch)) (router-listeners r route))))

(define (router-trigger! r route data)
  (define listeners (router-listeners r route))
  (for ([ch listeners])
    (async-channel-put ch data)))

(define (listener-get! l)
  (async-channel-try-get l))
