#lang racket

(require racket/async-channel)

(provide make-router
         router-add-listener!
         router-remove-listener!
         router-remove-listeners!
         router-trigger!)

(define (make-router)
  (make-hash))

(define (router-listeners r route)
  (hash-ref r route list))

(define (router-add-listener! r route cb)
  (hash-set! r route (append (router-listeners r route) (list cb))))

(define (router-remove-listener! r route cb)
  (hash-set! r route (filter (λ (n) (equal? n cb)) (router-listeners r route))))

(define (router-remove-listeners! r route)
  (hash-set! r route '()))

(define (router-trigger! r route data)
  (define listeners (router-listeners r route))
  (for ([cb listeners])
    (cb data)))
