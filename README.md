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

## Livecoding

TODO...

## API

TODO...

## TODO

* Tests
* Docs
* Markov chain
