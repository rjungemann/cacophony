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
; Record audio from ChucK
#| (engine-rec "presentation.wav") |#
; Set BPM
(set-bpm 140)

;; -----------
;; DSL #1: alm
;; -----------

; Make music sequences in an MML-like language.
(fluid-alm-run 1
  (alm o6 l8 a4 b c a4 c4 a4 b c g4 c4 a2))
(fluid-alm-run 1
  (alm o5 l4 a > g f2 < a > g f2 a2))

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
(define atmos #f)
(define bass-drum #f)
(define snare-drum #f)
(define closed-hh-evolver #f)
(define closed-hh #f)
(define bass #f)
(define lead #f)
(define lead-oct #f)

; Atmos
(come-in
  (define port-1 (random-port))
  (define port-2 (random-port))
  (define shred-1 (engine-sampler port-1 "examples/fade.wav" 1.0))
  (define shred-2 (engine-sampler port-2 "examples/swell.wav" 1.0))
  (define toggle (rotator (list #f #f #t #f #f #t #f #t)))
  (define sender-1 (add-sender "127.0.0.1" port-1))
  (define sender-2 (add-sender "127.0.0.1" port-2))
  (set! atmos
    (every (* 2 (1n))
      (if (toggle)
        (<< sender-1 #"/hit" 0.5)
        (<< sender-2 #"/hit" 0.5)))))

; Bass drum
(come-in
  (define port (random-port))
  (define shred (engine-sampler port "examples/tr-606/bass-drum.wav" 0.3))
  (define sender (add-sender "127.0.0.1" port))
  (set! bass-drum
    (every (4n)
      (<< sender #"/hit" 0.5))))

; Snare drum
(come-in
  (define port (random-port))
  (define shred (engine-sampler port "examples/tr-606/snare-drum.wav" 0.4))
  (define sender (add-sender "127.0.0.1" port))
  (define euclid (rotator (euclidian 3 8 2)))
  (define trig (rotator (list #t #t #t #t #t #f)))
  (set! snare-drum
    (every (8n)
      (when (and (euclid) (trig))
        (<< sender #"/hit" 0.1)))))

; Hi-hats
(come-in
  (define rhythm (box (list #t #f #f #t)))
  (define port (random-port))
  (define shred (engine-sampler port "examples/tr-606/closed-hh.wav" 0.55))
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
        (<< sender #"/hit" 0.05)))))

; Bass
(come-in
  (define port (random-port))
  (define shred (engine-synth port "examples/ensemble/AKWF_0001.wav" 0.5
                                   0.07 2000 400 0.05
                                   1 350 0.7 500
                                   15 550 0.7 400
                                   0.2))
  (define sender (add-sender "127.0.0.1" port))
  (define trig (rotator (list #t #t #t #t #f #t)))
  (define slide (rotator (list #f #f #t #f #f #f #f)))
  (define note (rotator '(24 24 27 24 27 36 22
                          24 24 27 24 34 36 22)))
  (set! bass
    (every (8n)
      (when (trig)
        (<< sender #"/key-on" (note) 0.5)
        (when (not (slide))
          (after (16n) (<< sender #"/key-off")))))))

; Lead
(come-in
  (define port (random-port))
  (define shred (engine-synth port "examples/ensemble/AKWF_0096.wav" 0.5
                                   0.07 2000 400 0.05
                                   1 350 0.6 700
                                   15 550 0.6 600
                                   0.6))
  (define trig (rotator (list #t #t #f #t #t #f #t #f
                              #t #t #f #t #t #f #f #f)))
  (define notes (list 36 43 39 43 36 39 43 36 44 43
                      36 34 39 39 46 48 34 36 39 36))
  (define octave (rotator (list 2 3)))
  (define current-octave 2)
  (define markov (make-markov notes 1))
  (define note (markov-random markov))
  (define (next-note)
    (define n note)
    (set! note (markov-next markov note))
    n)
  (define sender (add-sender "127.0.0.1" port))
  (set! lead-oct
    (every (* 4 (1n))
      (set! current-octave (octave))))
  (set! lead
    (every (8n)
      (when (trig)
        (<< sender #"/key-on" (+ (next-note) (* 12 current-octave)) 0.25)
        (after (16n) (<< sender #"/key-off"))))))

; Stop atmos
(remove-pulse-listener atmos)

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
(remove-pulse-listener lead-oct)
