; Making a
; ┌  ┌─┐┌─┐┌─┐┌─┐┌─┐┬ ┬┌─┐┌┐┌┬ ┬  ┐
; │  │  ├─┤│  │ │├─┘├─┤│ ││││└┬┘  │
; └  └─┘┴ ┴└─┘└─┘┴  ┴ ┴└─┘┘└┘ ┴   ┘
;                         with DSLs

; Make music
;
; with code
;
; on-the-fly
;
; with tailor-made languages.
;

; To run cacophony:
;
;     ./cacophony
;

;; ------------------
;; Starting Cacophony
;; ------------------

; Start the clock.
(start)
; Start the MIDI synth.
(fluid-start!)
; Start DSP system and add some libraries.
(engine-start!)
(engine-wavetable)
(engine-subtr)
(engine-info)
; Set BPM
(set-bpm 140)

;; -----------
;; DSL #1: alm
;; -----------

; Make music sequences in an MML-like language.
(fluid-alm-run
  (alm o6 l8 a4 b c a4 c4 a4 b c g4 f e))
(fluid-alm-run
  (alm o5 l4 a > g f2 < a > g f2))

;; ----------
;; DSL #2: ck
;; ----------

(engine-shredule-code
  (ck
    ; Load the wavetables.
    (array-@-decl 'SndBuf 'buffers (int 100))
    (for (=> (int 0) (decl 'int 'i)) (< 'i (int 100)) (inc 'i)
      (decl 'SndBuf 'buf)
      (=> (call 'Std.itoa (+ 'i (int 1))) (decl 'string 's))
      (=> (- (int 4) (call 's.length)) (decl 'int 'l))
      (for (=> (int 0) (decl 'int 'j)) (< 'j 'l) (inc 'j)
        (=> (+ (string "0") 's) 's))
      (=> (+ (string "examples/wavetables/AKWF_0001/AKWF_") 's (string ".wav")) 'buf.read)
      (@=> 'buf (array-ref 'buffers 'i)))
    ; Setup the wavetable oscillator.
    (decl 'Phasor 'phasor)
    (=> (float 55.0) 'phasor.freq)
    (decl 'Wavetable 'wavetable)
    (@=> 'buffers 'wavetable.buffers)
    (decl 'Gain 'gain)
    (=> (float 0.1) 'gain.gain)
    ; Begin morphing.
    (=> (float 0.0) (decl 'float 'n))
    (fun 'void 'morph '()
      (while true
        (=> (* (+ (call 'Math.sin 'n) (float 1.0)) (float 0.5)) 'wavetable.interp)
        (=> (+ 'n (float 0.00025)) 'n)
        (=> (dur (float 0.5) ms) now)))
    ; Play the oscillator.
    (=> 'phasor 'wavetable 'gain dac)
    (spork (call 'morph))
    (=> (dur (float 8000) ms) now)))

;; ----
;; Demo
;; ----

; Setup
(define bass-drum #f)
(define snare-drum #f)
(define closed-hh-evolver #f)
(define closed-hh #f)
(define bass #f)
(define lead #f)

; Bass drum
(next
  (define port (random-port))
  (define shred (engine-sampler port "examples/tr-606/bass-drum.wav"))
  (define sender (add-sender "127.0.0.1" port))
  (set! bass-drum
    (every (4n)
      (<< sender #"/hit" 0.5))))

; Snare drum
(next
  (define port (random-port))
  (define shred (engine-sampler port "examples/tr-606/snare-drum.wav"))
  (define sender (add-sender "127.0.0.1" port))
  (define euclid (rotator (euclidian 3 8 2)))
  (define trig (rotator (list #t #t #t #t #t #f)))
  (set! snare-drum
    (every (8n)
      (when (and (euclid) (trig))
        (<< sender #"/hit" 0.15)))))

; Hi-hats
(next
  (define rhythm (box (list #t #f #f #t)))
  (define port (random-port))
  (define shred (engine-sampler port "examples/tr-606/closed-hh.wav"))
  (define sender (add-sender "127.0.0.1" port))
  (define (rules n)
    (cond [(equal? n #t) (list #t #f)]
          [(equal? n #f) (list #t #t)]))
  (define i 0)
  (define (evolve)
    (set-box! rhythm (l-system (unbox rhythm) rules 1))
    (set! i 0))
  (define (tick)
    (define val (list-ref (unbox rhythm) i))
    (set! i (modulo (+ i 1) (length (unbox rhythm))))
    val)
  (set! closed-hh-evolver
    (every (* (1n) 4)
      (evolve)))
  (set! closed-hh
    (every (16n)
      (when (tick)
        (<< sender #"/hit" 0.10)))))

; Bass
(next
  (define port (random-port))
  (define shred (engine-synth port "examples/ensemble/AKWF_0001.wav" 0.5
                                   0.07 2000 400 0.05
                                   1 350 0.7 500
                                   15 550 0.7 400))
  (define sender (add-sender "127.0.0.1" port))
  (define trig (rotator (list #t #t #t #t #f #t)))
  (define slide (rotator (list #f #f #t #f #f #f #f)))
  (define note (rotator '(24 24 27 24 34 36 22)))
  (set! bass
    (every (8n)
      (when (trig)
        (<< sender #"/key-on" (note) 0.5)
        (when (not (slide))
          (after (16n) (<< sender #"/key-off")))))))

; Lead
(next
  (define port (random-port))
  (define shred (engine-synth port "examples/ensemble/AKWF_0096.wav" 0.5
                                   0.07 2000 400 0.05
                                   1 350 0.7 500
                                   15 550 0.7 400))
  (define trig (rotator (list #t #t #f #t #t #f #t #f
                              #t #t #f #t #t #f #f #f)))
  (define notes (list 36 43 39 43 36 39 43 36 44 43
                      36 34 39 39 46 48 34 36 39 36))
  (define markov (make-markov notes 1))
  (define note (markov-random markov))
  (define (next-note)
    (define n note)
    (set! note (markov-next markov note))
    n)
  (define sender (add-sender "127.0.0.1" port))
  (set! lead
    (every (8n)
      (when (trig)
        (<< sender #"/key-on" (+ (next-note) 12) 0.5)
        (after (16n) (<< sender #"/key-off"))))))

; Stop bass drum
(remove-pulse-listener bass-drum)

; Stop snare drum
(remove-pulse-listener snare-drum)

; Stop hi-hats
(remove-pulse-listener closed-hh-evolver)
(remove-pulse-listener closed-hh)

; Stop bass
(remove-pulse-listener bass)

; Stop lead
(remove-pulse-listener lead)
