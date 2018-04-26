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
; To throw a paragraph with TSlime: `vip<c-c><c-c>`
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
  (alm < l8 a4 b c g4 < a4 > a4 b c g4 f e))

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

; Bass Drum
(next
  (begin
    (define port (random-port))
    (define shred (engine-sampler port "examples/tr-606/bass-drum.wav"))
    (define s (add-sender "127.0.0.1" port))
    (every (4n)
      (<< s #"/hit" 0.5))))

; Bass
(next
  (begin
    (define port (random-port))
    (define shred (engine-synth port "examples/ensemble/AKWF_0001.wav" 0.5
                                     0.07 2000 400 0.05
                                     1 350 0.7 500
                                     15 550 0.7 400))
    (define sender (add-sender "127.0.0.1" port))
    (define trig (rotator (list #t #t #t #t #f #t)))
    (define slide (rotator (list #f #f #t #f #f #f #f)))
    (define note (rotator '(24 24 27 24 34 36 22)))
    (every (8n)
      (when (trig)
        (<< sender #"/key-on" (note) 0.5)
        (when (not (slide))
          (after (16n) (<< sender #"/key-off")))))))
