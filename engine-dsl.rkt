#lang racket

(require "utils.rkt"
         "engine.rkt"
         "ck.rkt")

(provide current-engine-chuck
         current-engine-chuck-args
         current-engine-chuck-port
         current-engine-osc-port
         current-engine-socket
         current-engine-channel
         current-engine-proc
         engine-start!
         engine-stop!
         engine-info
         engine-status
         engine-quit
         engine-shredule
         engine-replace
         engine-shreds
         engine-unshredule
         engine-shredule-code
         engine-wavetable
         engine-subtr
         engine-rec
         engine-sampler
         engine-synth)

(define current-engine-chuck
  (make-parameter
    (or (environment-variables-ref (current-environment-variables) #"CHUCK_PATH")
        "/usr/local/bin/chuck")))

(define current-engine-chuck-args
  (make-parameter
    (string-split
      (or (environment-variables-ref (current-environment-variables) #"CHUCK_ARGS")
          "")
      ";")))

(define current-engine-chuck-port
  (make-parameter #f))

(define current-engine-osc-port
  (make-parameter #f))

(define current-engine-socket
  (make-parameter #f))

(define current-engine-host
  (make-parameter "127.0.0.1"))

(define current-engine-proc
  (make-parameter #f))

(define current-engine-channel
  (make-parameter #f))

(define (engine-start!)
  (define c (unbox (current-engine-proc)))
  (when (not c)
    (set-box! (current-engine-proc) (engine-full-start! (current-engine-chuck)
                                                        (current-engine-chuck-args)
                                                        (current-engine-chuck-port)
                                                        (current-engine-osc-port)
                                                        (current-engine-channel)))))

(define (engine-stop!)
  (define c (unbox (current-engine-proc)))
  (when c
    (engine-full-stop! c (current-engine-socket))
    (set-box! (current-engine-proc) #f))
  (void))

(define (engine-receive!)
  (engine-full-receive! (current-engine-channel)))

(define (engine-send! route . args)
  (apply engine-full-send! (append (list (current-engine-socket)
                                         (current-engine-host)
                                         (current-engine-osc-port)
                                         route)
                                   args))
  (void))

(define (register-shred! id path)
  (engine-full-register-shred! (unbox (current-engine-proc)) id path))

(define (deregister-shred! id)
  (engine-full-deregister-shred! (unbox (current-engine-proc)) id))

(define (map-shreds ids)
  (let* ([c (unbox (current-engine-proc))]
         [shred-map (engine-proc-shreds c)]
         [new-shred-map (make-hash)])
    (for ([id ids])
      (cond
        [(= id 1)
         (void)]
        [(hash-has-key? shred-map id)
         (hash-set! new-shred-map id (hash-ref shred-map id))]
        [else
         (hash-set! new-shred-map id 'unknown)]))
    (set-engine-proc-shreds! c new-shred-map)
    new-shred-map))

(define (engine-status)
  (engine-send! #"/machine/status")
  (engine-receive!)
  (void))

(define (engine-shreds)
  (engine-send! #"/machine/shreds")
  (map-shreds (map string->number (regexp-match* #rx"[0-9]+" (engine-receive!)))))

(define (engine-shredule path)
  (if (file-exists? (first (string-split (bytes->string/utf-8 path) ":")))
    (begin
      (engine-send! #"/machine/add" path)
      (let ([id (string->number (engine-receive!))])
        (register-shred! id path)
        id))
    (begin
      (p "File not found!")
      -1)))

(define (engine-unshredule id)
  (engine-send! #"/machine/remove" id)
  (let ([id (string->number (engine-receive!))])
    (deregister-shred! id)
    id))

(define (engine-replace id path . args)
  (if (file-exists? (bytes->string/utf-8 path))
    (begin
      (engine-send! #"/machine/replace" id path)
      (let ([id (string->number (engine-receive!))])
        (register-shred! id (string-join (append (list (bytes->string/utf-8 path)) args) ":"))
        id))
    (begin
      (p "File not found!")
      0)))

(define (engine-quit)
  (engine-send! #"/quit")
  (engine-receive!)
  (void))

(define (engine-info)
  (p "─────")
  (p "ChucK")
  (p "─────")
  (p "PID ~a" (subprocess-pid (engine-proc-p (unbox (current-engine-proc)))))
  (p "TCP port ~a" (current-engine-chuck-port))
  (p "OSC port ~a" (current-engine-osc-port)))

; -----------
; Chuck Stuff
; -----------

(define wavetable-code
  (ck
    (public-class 'Wavetable 'Chugen
      (array-@-decl 'SndBuf 'buffers)
      (=> (float 0.0)
          (decl 'float 'interp))
      (fun 'float 'tick (list (decl 'float 'in))
        (=> (* 'interp (- (ref-call 'buffers 'size) (int 1)))
            (decl 'float 'interpolation))
        (=> (cast (ref-call 'Math 'floor 'interpolation) 'int)
            (decl 'int 'lowI))
        (=> (cast (+ 'lowI (int 1)) 'int)
            (decl 'int 'highI))
        (=> (- (* (- 'highI 'interpolation) (float 2.0)) (float 1.0))
            (decl 'float 'swing))
        (=> (ref-call 'Math 'sqrt (* (float 0.5) (+ (float 1.0) 'swing)))
            (decl 'float 'lowPercent))
        (=> (ref-call 'Math 'sqrt (* (float 0.5) (- (float 1.0) 'swing)))
            (decl 'float 'highPercent))
        (@=> (array-ref 'buffers 'lowI) (@-decl 'SndBuf 'low))
        (@=> (array-ref 'buffers 'highI) (@-decl 'SndBuf 'high))
        (=> (cast (* 'in (ref-call 'low 'samples)) 'int)
            (decl 'int 'lowSampleI))
        (=> (cast (* 'in (ref-call 'high 'samples)) 'int)
            (decl 'int 'highSampleI))
        (=> (ref-call 'low 'valueAt 'lowSampleI)
            (decl 'float 'lowValue))
        (=> (ref-call 'high 'valueAt 'highSampleI)
            (decl 'float 'highValue))
        (return (+ (* 'lowValue 'lowPercent) (* 'highValue 'highPercent)))))))

(define rec-code
  (ck
    (=> (ref-call 'me 'arg (int 0))
        (decl 'string 'filename))
    (=> dac
        (decl 'Gain 'g)
        (decl 'WvOut 'w)
        blackhole)
    (=> 'filename
        (ref 'w 'wavFilename))
    (<<<>>> (string "Writing to file:")
            (+ (string "'") (ref-call 'w 'filename) (string "'")))
    (=> (float 0.5)
        (ref 'g 'gain))
    (@=> null 'w)
    (while true
      (=> (dur (int 1) second) now))))

(define subtr-code
  (ck
    (public-class 'Subtr 'Chubgraph
      (decl 'float 'spread)
      (decl 'float 'filtermult)
      (decl 'float 'filteroffset)
      (decl 'SndBuf 'shape1)
      (decl 'SndBuf 'shape2)
      (decl 'ADSR 'ampenv)
      (decl 'ADSR 'filterenv)
      (decl 'LPF 'lpf)
      (=> inlet 'shape1 'lpf)
      (=> inlet 'shape2 'lpf)
      (=> 'lpf 'ampenv outlet)
      (=> (float 0.07) 'spread)
      (=> (int 1) (ref 'shape1 'loop))
      (=> (int 1) (ref 'shape2 'loop))
      (=> (float 0.5) (ref 'shape1 'gain))
      (=> (float 0.5) (ref 'shape2 'gain))
      (=> (float 1500.0) 'filtermult)
      (=> (float 200.0) 'filteroffset)
      (fun 'void 'read (list (decl 'string 's))
        (=> 's (ref 'shape1 'read))
        (=> 's (ref 'shape2 'read)))
      (fun 'void 'freq (list (decl 'float 'f))
        (=> 'f (ref 'Std 'ftom) (decl 'float 'm))
        (=> (+ 'm 'spread) (ref 'Std 'mtof) (ref 'shape1 'freq))
        (=> (- 'm 'spread) (ref 'Std 'mtof) (ref 'shape2 'freq)))
      (fun 'void 'keyOn '()
        (ref-call 'filterenv 'keyOn)
        (ref-call 'ampenv 'keyOn))
      (fun 'void 'keyOff '()
        (ref-call 'filterenv 'keyOff)
        (ref-call 'ampenv 'keyOff))
      (fun 'void 'envdrive '()
        (while true
          (=> (+ (* (ref-call 'filterenv 'last) 'filtermult) 'filteroffset)
              (ref 'lpf 'freq))
          (=> (dur 1 ms) now)))
      (spork (call 'envdrive)))))

; TODO: Strip path
(define engine-sampler-code
  (ck
    (=> (ref-call 'me 'arg (int 0))
        (ref 'Std 'atoi)
        (decl 'int 'port))
    (=> (ref-call 'me 'arg (int 1))
        (decl 'string 'path))
    (=> (ref-call 'me 'arg (int 2))
        (decl 'string 'filename))
    (=> (ref-call 'me 'arg (int 3))
        (ref 'Std 'atof)
        (decl 'float 'gain))
    (decl 'OscIn 'oin)
    (decl 'OscMsg 'msg)
    (=> 'port (ref 'oin 'port))
    (ref-call 'oin 'addAddress 'path)
    (decl 'SndBuf 'buf)
    (=> 'filename (ref 'buf 'read))
    (=> 'buf dac)
    (=> (int 0) (ref 'buf 'loop))
    (=> (int 0) (ref 'buf 'rate))
    (while true
      (=> 'oin now)
      (while (ref-call 'oin 'recv 'msg)
        (=> (int 0) (ref 'buf 'pos))
        (=> (float 'gain) (ref 'buf 'gain))
        (=> (float 1.0) (ref 'buf 'rate))))))

(define engine-synth-code
  (ck
    (=> (ref-call 'me 'arg (int 0))
        (ref 'Std 'atoi)
        (decl 'int 'port))
    (=> (ref-call 'me 'arg (int 1))
        (decl 'string 'filename))
    (=> (ref-call 'me 'arg (int 2))
        (ref 'Std 'atoi)
        (decl 'float 'spread))
    (=> (ref-call 'me 'arg (int 3))
        (ref 'Std 'atoi)
        (decl 'float 'filtermult))
    (=> (ref-call 'me 'arg (int 4))
        (ref 'Std 'atoi)
        (decl 'float 'filteroffset))
    (=> (ref-call 'me 'arg (int 5))
        (ref 'Std 'atoi)
        (decl 'float 'gain))
    (=> (ref-call 'me 'arg (int 6))
        (ref 'Std 'atoi)
        (decl 'float 'ampenvattack))
    (=> (ref-call 'me 'arg (int 7))
        (ref 'Std 'atoi)
        (decl 'float 'ampenvdecay))
    (=> (ref-call 'me 'arg (int 8))
        (ref 'Std 'atoi)
        (decl 'float 'ampenvsustain))
    (=> (ref-call 'me 'arg (int 9))
        (ref 'Std 'atoi)
        (decl 'float 'ampenvrelease))
    (=> (ref-call 'me 'arg (int 10))
        (ref 'Std 'atoi)
        (decl 'float 'filterenvattack))
    (=> (ref-call 'me 'arg (int 11))
        (ref 'Std 'atoi)
        (decl 'float 'filterenvdecay))
    (=> (ref-call 'me 'arg (int 12))
        (ref 'Std 'atoi)
        (decl 'float 'filterenvsustain))
    (=> (ref-call 'me 'arg (int 13))
        (ref 'Std 'atoi)
        (decl 'float 'filterenvrelease))

    (decl 'OscIn 'oin)
    (decl 'OscMsg 'msg)
    (=> 'port (ref 'oin 'port))
    (ref-call 'oin 'addAddress (string "/key-on,if"))
    (ref-call 'oin 'addAddress (string "/key-off"))
    (decl 'Subtr 's)
    (=> 'filename (ref 's 'read))
    (=> 's dac)
    (=> (float 36.0) (ref 'Std 'mtof) (ref 's 'freq))
    (ref-call 's.ampenv 'set (dur 'ampenvattack ms)
                             (dur 'ampenvdecay ms)
                             'ampenvsustain
                             (dur 'ampenvrelease ms))
    (ref-call 's.filterenv 'set (dur 'filterenvattack ms)
                                (dur 'filterenvdecay ms)
                                'filterenvsustain
                                (dur 'filterenvrelease ms))
    (=> (float 'filtermult) (ref 's 'filtermult))
    (=> (float 'filteroffset) (ref 's 'filteroffset))
    (while true
      (=> 'oin now)
      (while (ref-call 'oin 'recv 'msg)
        (if (equal? (ref 'msg 'address) (string "/key-on"))
          (=> (ref-call 'msg 'getFloat (int 1)) 's.gain)
          (ref-call 's 'keyOn))
        (if (equal? (ref 'msg 'address) (string "/key-off"))
          (ref-call 's 'keyOff))))))

(define (engine-shredule-code code . args)
  (tempfile
    code
    (λ (path)
      (define full-path
        (string-join (append (list (bytes->string/utf-8 path)) args) ":"))
      (engine-shredule (string->bytes/utf-8 full-path)))))

(define (engine-wavetable)
  (engine-shredule-code wavetable-code))

(define (engine-subtr)
  (engine-shredule-code subtr-code))

(define (engine-rec filename)
  (engine-shredule-code rec-code filename))

(define (engine-sampler port path filename gain)
  (engine-shredule-code engine-sampler-code
                        (number->string port)
                        path
                        filename
                        (number->string gain)))

(define (engine-synth . args)
  (apply engine-shredule-code (append (list engine-synth-code)
                                      (map (λ (n) (format "~a" n)) args))))
