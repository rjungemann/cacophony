#lang racket

(require osc)

(define current-chuck
  (make-parameter "/usr/local/bin/chuck"))

(define current-chuck-port
  (make-parameter #f))

(define current-osc-port
  (make-parameter #f))

(define current-socket
  (make-parameter #f))

(define current-host
  (make-parameter "127.0.0.1"))

(define current-chuck-proc
  (make-parameter #f))

(define current-channel
  (make-parameter #f))

(define (random-port)
  (define listener (tcp-listen 0))
  (define port #f)
  (call-with-values
    (lambda () (tcp-addresses listener #t))
    (lambda (a b c d) (set! port b)))
  (tcp-close listener)
  port)

(define (cyan s) (string-append "\u001b[36m" s "\u001b[0m"))
(define (blue s) (string-append "\u001b[34m" s "\u001b[0m"))
(define (yellow s) (string-append "\u001b[33m" s "\u001b[0m"))

(struct chuck-proc [p o i e oth eth] #:mutable)

(define (thread-output-handler o color)
  (define ch (current-channel))
  (thread
    (λ ()
       (define break? #f)
       (let loop ()
         (with-handlers ([exn:fail?
                           (lambda (_) (set! break? #t))])
           (define line (read-line o))
           (and (not (eof-object? line))
                (begin
                  (let ([m (regexp-match #rx"^\\[engine\\] (.*)$" line)])
                    (if m
                      (channel-put ch (last m))
                      (printf (color "~a\n") line))
                    (and (not break?) (loop))))))))))

(define (chuck-start!)
  (define-values (p o i e)
    (subprocess #f #f #f (current-chuck)
                         (format "--port ~a" (current-chuck-port))
                         (format "engine.ck:~a" (current-osc-port))))
  (define oth (thread-output-handler o blue))
  (define eth (thread-output-handler e yellow))
  (chuck-proc p o i e oth eth))

(define (chuck-stop!)
  (define c (current-chuck-proc))
  (close-output-port (chuck-proc-i c))
  (close-input-port (chuck-proc-o c))
  (close-input-port (chuck-proc-e c))
  (udp-close (current-socket))
  (subprocess-kill (chuck-proc-p c) #t)
  (kill-thread (chuck-proc-oth c))
  (kill-thread (chuck-proc-eth c)))

(define (p . args)
  (displayln (cyan (apply format args))))

(define (<< route . args)
  (udp-send-to (current-socket)
               (current-host)
               (current-osc-port)
               (osc-element->bytes (osc-message route args)))
  (void))

(define (>>)
  (channel-get (current-channel)))

(define (status)
  (<< #"/machine/status")
  (>>))

(define (shreds)
  (<< #"/machine/shreds")
  (map string->number (regexp-match* #rx"[0-9]+" (>>))))

(define (add path)
  (<< #"/machine/add" path)
  (>>))

(define (remove id)
  (<< #"/machine/remove" id)
  (>>))

(define (replace id path)
  (<< #"/machine/replace" id path)
  (>>))

(define (quit)
  (<< #"/quit")
  (>>))

; TODO: Make sure chuck quits on exit
; TODO: Make sure a file exists before adding and replacing
; TODO: Track running shreds
; TODO: REPL
; TODO: Socket REPL
; TODO: Rename functions so they don't conflict with cacophony
; TODO: Integrate into cacophony
(parameterize ([current-chuck-port (random-port)]
               [current-osc-port (random-port)]
               [current-socket (udp-open-socket)]
               [current-channel (make-channel)])
  (parameterize([current-chuck-proc (chuck-start!)])
    (p "chuck pid ~a" (subprocess-pid (chuck-proc-p (current-chuck-proc))))
    (p "chuck tcp port ~a" (current-chuck-port))
    (p "chuck osc port ~a" (current-osc-port))

    (>>)
    (status)
    (p (add #"examples/sample.ck"))
    (p (replace 2 #"examples/sample.ck"))
    (p "~a" (shreds))
    (p (remove 2))
    (quit)
    (chuck-stop!)))
