#lang racket

(provide
  (struct-out event-emitter)
  make-event-emitter
  trigger
  add-listener!
  remove-listener!
  clear-listeners!)

(struct event-emitter (listeners))

(define (make-event-emitter)
  (event-emitter (mutable-set)))

(define (trigger evt . args)
  (define l (set->list (event-emitter-listeners evt)))
  (for ([callback l])
    (apply callback args)))

(define (add-listener! evt callback)
  (set-add! (event-emitter-listeners evt) callback)
  callback)

(define (remove-listener! evt callback)
  (set-remove! (event-emitter-listeners evt) callback))

(define (clear-listeners! evt)
  (set-clear! (event-emitter-listeners evt)))
