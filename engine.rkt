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

(define engine-code #<<EOF
  1 => int running;
  OscIn oin;
  OscMsg msg;
  Std.atoi(me.arg(0)) => oin.port;
  oin.addAddress("/quit,N");
  oin.addAddress("/machine/status,N");
  oin.addAddress("/machine/shreds,N");
  oin.addAddress("/machine/add,s");
  oin.addAddress("/machine/remove,i");
  oin.addAddress("/machine/replace,is");

  chout <= "[engine] running" <= IO.newline();

  while (running != 0) {
    oin => now;
    while (oin.recv(msg) != 0) {
      if (msg.address == "/quit") {
        0 => running;
      }

      if (msg.address == "/machine/status") {
        Machine.status() => int n;
        chout <= "[engine] " <= n <= IO.newline();
      }

      if (msg.address == "/machine/shreds") {
        Machine.shreds() @=> int shreds[];
        chout <= "[engine] [";
        for (0 => int i; i < shreds.size(); i++) {
          chout <= shreds[i];
          if (i < shreds.size() - 1) { chout <= ", "; }
        }
        chout <= "]" <= IO.newline();
      }

      if (msg.address == "/machine/add") {
        chout <= "[engine] " <= Machine.add(msg.getString(0)) <= IO.newline();
      }

      if (msg.address == "/machine/remove") {
        chout <= "[engine] " <= Machine.remove(msg.getInt(0)) <= IO.newline();
      }

      if (msg.address == "/machine/replace") {
        chout <= "[engine] " <= Machine.replace(msg.getInt(0), msg.getString(1)) <= IO.newline();
      }
    }
  }

  chout <= "[engine] quit" <= IO.newline();
EOF
)

(define (open-engine-path cb)
  (define path (make-temporary-file))
  (define o (open-output-file	path #:mode 'text #:exists 'truncate))
  (displayln 2)
  (display engine-code o)
  (close-output-port o)
  (displayln 3)
  (cb path)
  (delete-file path)
  (void))

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
  #| (when (not (file-exists? (bytes->string/utf-8 (current-engine-path)))) |#
  #|   (error "Could not find ~a!" (current-engine-path))) |#

  (displayln 1)
  (open-engine-path
    (λ (path)
      (displayln path)
      (define-values (p o i e)
        (apply subprocess (list #f #f #f (current-engine-chuck)
                                (format "--port ~a" (current-engine-chuck-port))
                                (format "~a:~a" path (current-engine-osc-port)))))
      (define oth (thread-output-handler o blue))
      (define eth (thread-output-handler e yellow))
      (c->) ; Wait for first message from chuck.
      (set-box! (current-engine-chuck-proc) (chuck-proc p o i e oth eth (make-hash))))))

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
  (c->)
  (void))

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
