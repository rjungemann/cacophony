#lang racket

(provide
  bpm->ms
  ppqn->ms
  (struct-out clock)
  make-clock
  now
  clock-tick!
  clock-start!
  clock-synchronize!
  clock-at!
  clock-on!
  clock-after!
  clock-immediately!
  set-clock-bpm-ppqn!
  clock-beats
  clock-ppqns
  clock-every-beat!
  clock-every-ppqn!
  clock-clear!
  stop!)

(struct clock (events started-at bpm ppqn clearing? mutex) #:mutable)

(define (bpm->ms bpm)
  (/ 60000.0 bpm))

(define (ppqn->ms bpm ppqn)
  (/ 60000.0 (* bpm ppqn)))

(define (make-clock bpm ppqn)
  (clock '() -1 bpm ppqn #f (make-semaphore 1)))

(define (now)
  (current-inexact-milliseconds))

(define (clock-event-reducer c t)
  (lambda (n acc)
    (if (<= (first n) t)
        (begin
          (apply (second n) (list c t))
          acc)
        (append acc (list n)))))

(define (clock-tick! c)
  (call-with-semaphore
    (clock-mutex c)
    (λ ()
      ; If clock is to be cleared, then clear it before ticking.
      (if (clock-clearing? c)
        (set-clock-events! c '())
        (set-clock-clearing?! c #f))
      ; Handle events.
      (let ([events (clock-events c)])
        ; This is so `clock-at!` and `clock-on!` will register new events.
        (set-clock-events! c '())
        (set-clock-events! c (append (foldl (clock-event-reducer c (now)) '() events)
                                     (clock-events c)))))))

(define (clock-synchronize! c cb)
  (call-with-semaphore
    (clock-mutex c)
    (λ ()
      (cb c))))

(define (clock-at! c time cb)
  (set-clock-events! c (append (clock-events c) (list (list time cb)))))

(define (clock-on! c beats ppqns cb)
  (define bpm (clock-bpm c))
  (define ppqn (clock-ppqn c))
  (define started-at (clock-started-at c))
  (define time (+ started-at
                  (+ (* (bpm->ms bpm) beats)
                     (* (ppqn->ms bpm ppqn) ppqns))))
  (clock-at! c time cb))

(define (clock-after! c after-beats after-ppqns cb)
  (define bpm (clock-bpm c))
  (define ppqn (clock-ppqn c))
  (define t (clock-started-at c))
  (define beats (clock-beats c t))
  (define ppqns (clock-ppqns c t))
  (define next-raw-ppqn (+ (* (+ after-beats beats) ppqn) (+ after-ppqns ppqns)))
  (define next-beat (+ after-beats beats))
  (define next-ppqn (modulo next-raw-ppqn ppqn))
  (clock-on! c next-beat next-ppqn cb))

(define (clock-immediately! c cb)
  (clock-at! c (now) cb))

(define (clock-start! c)
  (define stopper (box #t))
  (thread
    (λ ()
      (let loop ()
        (and (unbox stopper) (clock-tick! c))
        (loop))))
  (set-clock-started-at! c (now))
  stopper)

(define (set-clock-bpm-ppqn! c bpm ppqn)
  (set-clock-bpm! bpm)
  (set-clock-ppqn! ppqn)
  (set-clock-started-at! c (now)))

(define (clock-beats c t)
  (define bpm (clock-bpm c))
  (define started-at (clock-started-at c))
  (define since (- t started-at))
  (floor (/ since (bpm->ms bpm))))

(define (clock-ppqns c t)
  (define bpm (clock-bpm c))
  (define ppqn (clock-ppqn c))
  (define started-at (clock-started-at c))
  (define since (- t started-at))
  (modulo (floor (/ since (ppqn->ms bpm ppqn))) ppqn))

(define (clock-every-beat! c cb)
  (define stopper (box #t))
  (define (every-beat c t)
    (cb c t)
    (and (unbox stopper)
         (clock-on! c (+ 1 (clock-beats c t)) 0 every-beat)))
  (clock-on! c (+ 1 (clock-beats c (now))) 0 every-beat)
  stopper)

(define (clock-every-ppqn! c cb)
  (define stopper (box #t))
  (define t (now))
  (define ppqn (clock-ppqn c))
  (define beats (clock-beats c t))
  (define ppqns (clock-ppqns c t))
  (define next-raw-ppqn (+ (* beats ppqn) (+ 1 ppqns)))
  (define next-ppqn (modulo next-raw-ppqn ppqn))
  (define next-beat (floor (/ next-raw-ppqn ppqn)))
  (define (every-ppqn c t)
    (define ppqn (clock-ppqn c))
    (define beats (clock-beats c t))
    (define ppqns (clock-ppqns c t))
    (define next-raw-ppqn (+ (* beats ppqn) (+ 1 ppqns)))
    (define next-ppqn (modulo next-raw-ppqn ppqn))
    (define next-beat (floor (/ next-raw-ppqn ppqn)))
    (cb c t)
    (and (unbox stopper)
         (clock-on! c next-beat next-ppqn every-ppqn)))
  (clock-on! c next-beat next-ppqn every-ppqn)
  stopper)

(define (clock-clear! c)
  (set-clock-clearing?! c #t))

(define (stop! b)
  (set-box! b #f))
