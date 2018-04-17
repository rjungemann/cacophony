# Cacophony

By Roger Jungemann

A livecoding system in Racket. Juggle OSC connections and schedule events.

Intended to be used with TSlime.vim.

## Setup

```sh
# Make sure you have Racket installed.
brew install minimal-racket

# Some optional stuff you can install.
brew install rlwrap chuck

# Install dependencies.
raco setup
# Or...
yes | raco pkg install --skip-installed rx osc scribble

# Try running.
racket racket.rkt
# Or try running it in `rlwrap`:
rlwrap -S '' racket main.rkt

# Then try typing `(+ 1 1)`, and it should respond `2`.
# Hit ctrl-c to quit.

# Try building.
raco exe -o cacophony main.rkt

# Then try running that.
./cacophony

# Build docs.
raco pkg install
```

## Examples

Either run the "Tester" Max project, or

```
chuck examples/tester.ck
```

Then:

```racket
; Start the clock.
(start)

; Define a receiver and sender.
(define r (add-receiver 13699))
(define s (add-sender "127.0.0.1" 13698))

; Schedule an event to be run next tick.
(defer (λ (_) (p "Now starting!n")))

; Schedule some events to be run every 2.5 beats and every beat.
(every (+ (2n) (8n)) (λ (_) (p "Every 2.5 beats")))
(every (4n) (λ (_) (p "Every beat\n")))

; Wait for a response.
(>> r #"/status" (λ (m) (p "Received ~a" m)))

; Send out a message. The above code should receive a response.
(<< s #"/status")
```

Either run the "Tester 2" Max project, or

```
chuck examples/ensemble/{subtr.ck,bass-drum.ck,snare-drum.ck,bass.ck,lead.ck}
```

Then:

```racket
; Start the clock.
(start)

; Change the tempo.
(set-bpm 140)

; Define a sender.
(define s (add-sender "127.0.0.1" 13698))

; Schedule the drums.
(define snare? (rotator (list #f #t)))
(every (4n)
  (λ (_)
    (<< s #"/bass-drum")
    (and (snare?) (<< s #"/snare-drum")) ))

; Schedule the bass and lead.
(define bass (rotator '(24 24 27 24 34 36 22)))
(define lead (rotator '(48 50 51 48 50 46 53)))
(every (8n)
  (λ (_)
    (<< s #"/bass" (bass))
    (<< s #"/lead" (lead))))
```

Dynamically scaling BPM with linear interpolation.

```racket
(define l (lerper 120 60 0.1))

(every (4n)
  (λ (_)
    (p "Tick!")
    (set-bpm (l))))
```

L-system beats. Evolves every 4 measures.

```racket
(define rhythm (box (list #t #f #f #t)))

(define (rules n)
  (cond [(equal? n #t) (list #t #f)]
        [(equal? n #f) (list #f #t)]))

(define n 0)

(define (evolve)
  (set-box! (l-system (unbox rhythm) rules 1))
  (set! n 0))

(define (tick)
  (define val (= 0 (modulo n (length (unbox rhythm)))))
  (set! n (+ 1 n))
  val)

(every (16n)
  (λ (_)
    (and (tick) (p "Tick!"))))

(every (* (1n) 4)
  (λ (_)
    (p "Evolving...")
    (evolve))
```

Markov melodies. Try it with rhythms too!

```racket
(define s (add-sender "127.0.0.1" 13698))

(define notes (list 37 38 37 39 38 40 39 38 37 39 41 37 38 41 39))
(define m (make-markov notes 1))
(define note (markov-random m))
(define (next-note)
  (define n note)
  (markov-next m note)
  n)

(every (8n)
  (λ (_)
    (<< s #"/bass" (next-note))))
```

Managing ChucK:

```racket
(engine-start!)
(engine-info)

(p "~a" (engine-shredule #"examples/sample.ck"))
(p "~a" (engine-replace 2 #"examples/sample.ck"))
(p "~a" (engine-shreds))
(p "~a" (engine-unshredule 2))

(engine-quit)
(engine-stop!)
```

Managing Fluidsynth:

```racket
(fluid-start!)
(fluid-send! "noteon 1 48 64")
(fluid-flush!)
(sleep 2)
(fluid-stop!)
```

## Using the socket

TODO...

## Livecoding with TSlime.vim

TODO...

## API

TODO...

## TODO

* Tests
* Docs
* Environment variables
* Break out goodies into their own file
* Add timeouts to engine calls
