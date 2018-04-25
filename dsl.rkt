#lang racket

(require compatibility/defmacro
         osc
         unix-signals
         "utils.rkt"
         "events.rkt"
         "clock.rkt"
         "receiver.rkt"
         "sender.rkt"
         "socket.rkt"
         "ck.rkt"
         "engine-dsl.rkt"
         "alm.rkt")

(provide (all-defined-out))

(define current-receivers
  (make-parameter (box (list))))

(define current-senders
  (make-parameter (box (list))))

(define current-clock
  (make-parameter #f))

(define current-stopper
  (make-parameter (box #f)))

(define current-ich
  (make-parameter (make-channel)))

(define current-och
  (make-parameter (make-channel)))

(define socket-port
  (make-parameter 1234))

(define current-sleep-time
  (make-parameter 0.001))

(define (add-receiver port)
  (define receiver (make-receiver port))
  (receiver-start! receiver)
  (set-box! (current-receivers) (append (unbox (current-receivers)) (list receiver)))
  receiver)

(define (remove-receiver receiver)
  (receiver-stop! receiver)
  (set-box! (current-receivers) (filter (λ (n) (equal? receiver n)) (unbox (current-receivers))))
  (void))

(define (add-sender host port)
  (define sender (make-sender host port))
  (set-box! (current-senders) (append (unbox (current-senders)) (list sender)))
  sender)

(define (remove-sender sender)
  (sender-stop! sender)
  (set-box! (current-senders) (filter (λ (n) (equal? sender n)) (unbox (current-senders))))
  (void))

(define (prepare)
  (define ich (current-ich))
  (define och (current-och))
  (define receivers (current-receivers))
  (define namespace (current-namespace))
  (define prompt-read (current-prompt-read))
  (define sleep-time (current-sleep-time))

  ; Start the socket server.
  (let ([ich (current-ich)]
        [och (current-och)]
        [port (socket-port)])
    (thread
      (lambda ()
        (socket-start! port ich och))))

  ; Check for SIGINT and exit.
  (capture-signal! 'SIGINT)
  (thread
    (λ ()
      (let ([signum (read-signal)])
        (printf "~a ~a\n" signum (lookup-signal-name signum))
        (when (equal? 'SIGINT (lookup-signal-name signum))
          (exit 130)))))

  (thread
    (λ ()
      (let loop ([n 0])
        ; Tell GC to run in incremental mode (must be called sometime after
        ; every major collection).
        (when (= 0 (modulo n 2000000))
          (collect-garbage 'incremental))
        ; Receiver messages.
        (for ([receiver (unbox receivers)])
          (receiver-tick! receiver))
        ; Socket eval.
        (parameterize ([current-namespace namespace]
                       [current-prompt-read prompt-read])
          (socket-eval! ich och))
        (sleep sleep-time)
        (loop (+ n 1)))))

  (void))

(define (start)
  (define clock (current-clock))
  (define stopper (current-stopper))
  (define sleep-time (current-sleep-time))

  (set-box! stopper #t)
  (clock-start! clock)
  (thread
    (λ ()
      (let loop ()
        (clock-tick! clock)
        (sleep sleep-time)
        (when (unbox stopper) (loop)))))
  (void))

(define (stop)
  (set-box! (current-stopper) #f)
  (collect-garbage 'major)
  (void))

(define (clear)
  (clock-clear! (current-clock))
  (void))

(define (remove-tick-listener cb)
  (remove-listener! (clock-tick-vent (current-clock)) cb)
  (void))

(define (remove-pulse-listener cb)
  (remove-listener! (clock-pulse-vent (current-clock)) cb)
  (void))

(define (help)
  (p "TODO"))

(define (splash)
  (displayln (string-append (yellow "┌  ┌─┐┌─┐┌─┐┌─┐┌─┐┬ ┬┌─┐┌┐┌┬ ┬  ┐  ") (magenta "Scheme + TSlime.vim")))
  (displayln (string-append (yellow "│  │  ├─┤│  │ │├─┘├─┤│ ││││└┬┘  │  ") (magenta "Livecoding         ")))
  (displayln (string-append (yellow "└  └─┘┴ ┴└─┘└─┘┴  ┴ ┴└─┘┘└┘ ┴   ┘  ") (magenta "Platform           ")))
  (printf "\n")
  (p "Socket server running on port ~a." (socket-port))
  (printf "\n")
  (p "Type in commands to get started. Common commands:")
  (p "(help) (status) (start) (stop)")
  (printf "\n"))

(define (start-repl anc)
  (parameterize ([current-namespace (namespace-anchor->namespace anc)]
                 [current-prompt-read (λ ()
                                        (let ([in ((current-get-interaction-input-port))])
                                          ((current-read-interaction) (object-name in) in)))]
                 [current-receivers (box (list))]
                 [current-clock (make-clock 120.0 12.0)])
    (splash)
    (prepare)

    ; Engine parameters
    (parameterize ([current-engine-chuck-port (random-port)]
                   [current-engine-osc-port (random-port)]
                   [current-engine-socket (udp-open-socket)]
                   [current-engine-channel (make-channel)]
                   [current-subprocess-custodian-mode 'kill])
      (parameterize([current-engine-proc (box #f)])
        ; Take a deep breath...
        (collect-garbage 'major)
        ; And go!
        (read-eval-print-loop)))))

(define-macro (defer . body)
  `(clock-at! (current-clock) (now) (λ (_) ,@body)))

(define-macro (next . body)
  `(clock-next-beat! (current-clock) (λ (_) ,@body)))

(define-macro (after beats . body)
  `(clock-after! (current-clock) ,beats (λ (_) ,@body)))

(define-macro (every beats . body)
  `(clock-every! (current-clock) ,beats (λ (_) ,@body)))

(define (<< s route . args)
  (sender-send! s (osc-message route args))
  (void))

(define (>> r route cb)
  (receiver-add-listener! r route cb))

(define (>* r route cb)
  (receiver-remove-listener! r route cb))

(define (router-remove-listener r route cb)
  (receiver-router-remove-listener! r route cb)
  (void))

(define (router-remove-listeners r route)
  (receiver-router-remove-listener! r route)
  (void))

(define (router-clear r)
  (receiver-router-clear! r)
  (void))

(define (set-bpm n)
  (set-clock-bpm! (current-clock) n)
  (void))

(define (set-ppqn n)
  (set-clock-ppqn! (current-clock) n)
  (void))

(define (beats)
  (clock-beat (current-clock)))

(define (pulses)
  (clock-pulse (current-clock)))

(define (beats->ppqn beats)
  (* (clock-ppqn (current-clock)) beats))

(define (status)
  (define receivers (unbox (current-receivers)))
  (define senders (unbox (current-senders)))
  (p "─────────")
  (p "Receivers")
  (p "─────────")
  (for ([r receivers])
    (p ":~a" (receiver-port r))
    (define router (receiver-router r))
    (for ([(route listeners) router]
          [i (in-naturals)])
      (if (= (+ 1 i) (hash-count router))
        (p "  └ ~a (~a listeners)" route (length listeners))
        (p "  ├ ~a (~a listeners)" route (length listeners)))))
  (when (empty? receivers)
    (p "No receivers!"))
  (printf "\n")
  (p "───────")
  (p "Senders")
  (p "───────")
  (for ([s senders])
    (p "~a:~a" (sender-host s) (sender-port s)))
  (when (empty? senders)
    (p "No senders"))
  (printf "\n")
  (p "─────")
  (p "Stats")
  (p "─────")
  (p "Socket port ~a" (socket-port))
  (p "Running? ~a" (unbox (current-stopper)))
  (p "BPM ~a" (clock-bpm (current-clock)))
  (p "PPQN ~a" (clock-ppqn (current-clock)))
  (p "Beats/Pulses ~a ~a" (beats) (pulses)))

(define stats-vector (make-vector 12))
(define stats-names (list 'current-process-milliseconds
                          'current-milliseconds
                          'current-gc-milliseconds
                          'gc-count
                          'thread-context-switches
                          'internal-stack-overflows
                          'threads-scheduled
                          'syntax-objects-read
                          'hash-tables-searched
                          'additional-hash-slots-searched
                          'bytes-allocated-not-reported
                          'peak-allocated-bytes))
(define (memory)
  (dump-memory-stats)
  (vector-set-performance-stats! stats-vector)
  (for ([n stats-names]
        [o stats-vector])
    (printf "~a: ~a\n" n o)))

; =======
; Helpers
; =======

; ---------
; Intervals
; ---------

(define (1nd) 6)
(define (1n) 4)
(define (1nt) (/ 8.0 3.0))
(define (2nd) 3)
(define (2n) 2)
(define (2nt) (/ 4.0 3.0))
(define (4nd) 1.5)
(define (4n) 1)
(define (4nt) (/ 2.0 3.0))
(define (8nd) 0.75)
(define (8n) 0.5)
(define (8nt) (/ 1.0 3.0))
(define (16nd) 0.375)
(define (16n) 0.25)
(define (16nt) (/ 1.0 6.0))
(define (32nd) 0.1875) ; NOTE: Can't be used at 24 ppqn!
(define (32n) 0.125)
(define (32nt) (/ 1.0 12.0))
(define (64nd) 0.09375) ; NOTE: Can't be used at 24 ppqn!
(define (64n) 0.0625) ; NOTE: Can't be used at 24 ppqn!
(define (64nt) (/ 1.0 24.0))

; ------
; Basics
; ------

(define (rotate-left l n)
  (append (drop l n) (take l n)))

(define (rotate-right l n)
  (append (take-right l n) (drop-right l n)))

; -------
; Rotator
; -------

(define (generator notes)
  (define i 0)
  (λ ()
    (if (< i (- (length notes) i))
      (let ([n (list-ref notes i)])
        (define n (list-ref notes i))
        (set! i (+ i 1))
        n)
      #f)))

(define (rotator l)
  (define n 0)
  (λ ()
    (define v (list-ref l (modulo n (length l))))
    (set! n (+ n 1))
    v))

; -------------
; Interpolation
; -------------

(define (lerp v w t)
  (cond [(< t 0.0) v]
        [(> t 1.0) w]
        [else (+ (* (- 1 t) v) (* t w))]))

(define (lerper v w incr)
  (define t 0)
  (λ ()
    (define v (lerp v w t))
    (set! t (+ incr t))
    v))

; ---------
; L-systems
; ---------

(define (l-system current rules n)
  (for/fold ([accum current])
            ([i (in-range 0 n)])
    (flatten (map rules accum))))

(define (euclidian num-pulses total-steps)
  (define (generate-chunk n)
    (for/fold ([l '()])
              ([i (in-range 0 n)])
      (append l (list 0))))
  (define steps '())
  (define bucket '())
  (for ([i (in-range 0 total-steps)])
    (set! bucket (append bucket (generate-chunk num-pulses)))
    (if (>= (length bucket) total-steps)
      (begin
        (set! bucket (drop bucket total-steps))
        (set! steps (append steps (list 1))))
      (set! steps (append steps (list 0)))))
  steps)

; ------
; Markov
; ------

(struct markov [data depth lookup])

(define (make-markov data depth)
  (define d (+ depth 1))
  (define lookup
    (for/fold ([accum (hash)])
              ([i (in-range 0 (- (length data) d -1))])
      (define focus (take (drop data i) d))
      (define key (drop-right focus 1))
      (define value (last focus))
      (define values
        (if (hash-has-key? accum key)
            (append (hash-ref accum key) (list value))
            (list value)))
      (hash-set accum key values)))
  (markov data depth lookup))

(define (markov-next m . args)
  (first (shuffle (hash-ref (markov-lookup m) args))))

(define (markov-random m)
  (first (shuffle (markov-data m))))

; ---
; Alm
; ---

(define (alm-run notes cb)
  (for ([note notes])
    (when (or (equal? (first note) 'noteon)
              (equal? (first note) 'noteoff))
      (after (last note) (λ (e) (cb e note))))))
