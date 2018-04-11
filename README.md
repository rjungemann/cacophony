# Cacophony

By Roger Jungemann

A livecoding system in Racket. Juggle OSC connections and schedule events.

Intended to be used with TSlime.vim.

## Setup

```sh
# Make sure you have Racket installed.
brew install minimal-racket

# Install dependencies.
raco setup
# Or...
yes | raco pkg install --skip-installed rx osc scribble

# Try running.
racket cacophony.rkt
# Then try typing `(+ 1 1)`, and it should respond `2`.
# Hit ctrl-d to quit.

# Try building.
raco exe -o cacophony main.rkt

# Then try running that.
./cacophony

# Build docs.
raco pkg install
```

## Example

Run the "Tester" Max project, then:

```racket
; Start the clock.
(start)

; Define a receiver and sender.
(define r (add-receiver 13699))
(define s (add-sender "127.0.0.1" 13698))

; Schedule an event to be run next tick.
(defer (λ (e) (printf "Now starting!\n")))

; Schedule some events to be run every 2.5 beats and every beat.
(every (+ (2n) (8n)) (λ (e) (printf "Every 2.5 beats\n")))
(every (4n) (λ (e) (printf "Every beat\n")))

; Wait for a response.
(>> r #"/status" (λ (m) (printf "Received ~a\n" m)))

; Send out a message. The above code should receive a response.
(<< s #"/status")
```

Run the "Tester 2" Max project, then:

```racket
; Start the clock.
(start)

; Change the tempo.
(set-bpm 140)

; Define a sender.
(define s (add-sender "127.0.0.1" 13698))

; Schedule the drums.
(define snare? (rotate (list #f #t)))
(every (4n) (λ (e)
  (<< s #"/bass-drum")
  (and (snare?) (<< s #"/snare-drum")) ))

; Schedule the bass and lead.
(define bass (rotate '(24 24 27 24 34 36 22)))
(define lead (rotate '(48 50 51 48 50 46 53)))
(every (8n) (λ (e)
  (<< s #"/bass" (bass))
  (<< s #"/lead" (lead))))
```

Dynamically scaling BPM with linear interpolation.

```racket
(define l (lerper 120 60 0.1))
(every (4n) (λ (e)
  (p "Tick!")
  (set-bpm (l))))
```

## Livecoding

TODO...

## API

TODO...

## TODO

* TCP eval Server
* Tests
* Docs
* L-Tree
* Markov chain
* Euclidian sequencer
