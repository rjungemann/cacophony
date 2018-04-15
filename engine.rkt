#lang racket

(require osc
         "utils.rkt")

(provide current-engine-chuck
         current-engine-path
         current-engine-chuck-port
         current-engine-osc-port
         current-engine-socket
         current-engine-channel
         current-engine-chuck-proc
         random-port
         chuck-start!
         chuck-stop!
         chuck-info
         chuck-status
         chuck-quit
         shredule
         replace
         shreds
         unshredule)

(define current-engine-chuck
  (make-parameter "/usr/local/bin/chuck"))

(define current-engine-path
  (make-parameter "engine.ck"))

(define current-engine-chuck-port
  (make-parameter #f))

(define current-engine-osc-port
  (make-parameter #f))

(define current-engine-socket
  (make-parameter #f))

(define current-engine-host
  (make-parameter "127.0.0.1"))

(define current-engine-chuck-proc
  (make-parameter #f))

(define current-engine-channel
  (make-parameter #f))

(struct chuck-proc [p o i e oth eth shreds] #:mutable)

(define (thread-line-handler ch color line)
  (let ([m (regexp-match #rx"^\\[engine\\] (.*)$" line)])
    (if m
      (channel-put ch (last m))
      (printf (color "~a\n") line))))

(define (thread-output-handler o color)
  (define ch (current-engine-channel))
  (thread
    (λ ()
      (let loop ()
        (define line
          (with-handlers ([exn:fail?
                           (λ (_) eof)])
            (read-line o)))
        (when (not (eof-object? line))
          (begin
            (thread-line-handler ch color line)
            (loop)))))))

(define (chuck-start!)
  (when (not (file-exists? (bytes->string/utf-8 (current-engine-path))))
    (error "Could not find ~a!" (current-engine-path)))

  (define-values (p o i e)
    (apply subprocess (list #f #f #f (current-engine-chuck)
                            (format "--port ~a" (current-engine-chuck-port))
                            (format "engine.ck:~a" (current-engine-osc-port)))))
  (define oth (thread-output-handler o blue))
  (define eth (thread-output-handler e yellow))
  (c->) ; Wait for first message from chuck.
  (set-box! (current-engine-chuck-proc) (chuck-proc p o i e oth eth (make-hash))))

(define (chuck-stop!)
  (define c (unbox (current-engine-chuck-proc)))
  (when c
    (begin
      (close-output-port (chuck-proc-i c))
      (close-input-port (chuck-proc-o c))
      (close-input-port (chuck-proc-e c))
      (when (not (udp-bound? (current-engine-socket)))
        (udp-close (current-engine-socket)))
      (subprocess-kill (chuck-proc-p c) #t)
      (kill-thread (chuck-proc-oth c))
      (kill-thread (chuck-proc-eth c))
      (set-box! (current-engine-chuck-proc) #f)))
  (void))

(define (c<- route . args)
  (udp-send-to (current-engine-socket)
               (current-engine-host)
               (current-engine-osc-port)
               (osc-element->bytes (osc-message route args)))
  (void))

(define (register-shred! id path)
  (hash-set! (chuck-proc-shreds (unbox (current-engine-chuck-proc))) id path))

(define (deregister-shred! id)
  (hash-remove! (chuck-proc-shreds (unbox (current-engine-chuck-proc))) id))

(define (map-shreds ids)
  (let* ([c (unbox (current-engine-chuck-proc))]
         [shred-map (chuck-proc-shreds c)]
         [new-shred-map (make-hash)])
    (for ([id ids])
      (cond
        [(= id 1)
         (void)]
        [(hash-has-key? shred-map id)
         (hash-set! new-shred-map id (hash-ref shred-map id))]
        [else
         (hash-set! new-shred-map id 'unknown)]))
    (set-chuck-proc-shreds! c new-shred-map)
    new-shred-map))

(define (c->)
  (channel-get (current-engine-channel)))

(define (chuck-status)
  (c<- #"/machine/status")
  (c->))

(define (shreds)
  (c<- #"/machine/shreds")
  (map-shreds (map string->number (regexp-match* #rx"[0-9]+" (c->)))))

(define (shredule path)
  (if (file-exists? (bytes->string/utf-8 path))
    (begin
      (c<- #"/machine/add" path)
      (let ([id (string->number (c->))])
        (register-shred! path id)
        id))
    (begin
      (p "File not found!")
      -1)))

(define (unshredule id)
  (c<- #"/machine/remove" id)
  (let ([id (string->number (c->))])
    (deregister-shred! id)
    id))

(define (replace id path)
  (if (file-exists? (bytes->string/utf-8 path))
    (begin
      (c<- #"/machine/replace" id path)
      (let ([id (string->number (c->))])
        (register-shred! id path)
        id))
    (begin
      (p "File not found!")
      0)))

(define (chuck-quit)
  (c<- #"/quit")
  (c->))

(define (chuck-info)
  (p "─────")
  (p "ChucK")
  (p "─────")
  (p "PID ~a" (subprocess-pid (chuck-proc-p (unbox (current-engine-chuck-proc)))))
  (p "TCP port ~a" (current-engine-chuck-port))
  (p "OSC port ~a" (current-engine-osc-port)))
