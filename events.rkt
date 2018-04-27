#lang racket

(provide
  (struct-out event-emitter)
  make-event-emitter
  trigger
  add-listener!
  remove-listener!
  clear-listeners!)

(struct event-emitter (listeners mutex))

(define (make-event-emitter)
  (event-emitter (mutable-set) (make-semaphore 1)))

(define (trigger evt . args)
  (call-with-semaphore (event-emitter-mutex evt)
    (λ ()
      (for ([callback (event-emitter-listeners evt)])
        (apply callback args)))))

(define (add-listener! evt callback)
  (call-with-semaphore (event-emitter-mutex evt)
    (λ ()
      (set-add! (event-emitter-listeners evt) callback)))
  callback)

(define (remove-listener! evt callback)
  (call-with-semaphore (event-emitter-mutex evt)
    (λ ()
      (set-remove! (event-emitter-listeners evt) callback))))

(define (clear-listeners! evt)
  (call-with-semaphore (event-emitter-mutex evt)
    (λ ()
      (set-clear! (event-emitter-listeners evt)))))
