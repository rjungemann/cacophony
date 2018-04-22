#lang racket

(require osc
         racket/sandbox
         "utils.rkt"
         "ck.rkt")

(provide engine-full-start!
         engine-full-stop!
         engine-full-send!
         engine-full-receive!
         engine-full-register-shred!
         engine-full-deregister-shred!
         (struct-out engine-proc))

(define engine-code
  (ck
    (=> (int 1) (decl 'int 'running))
    (decl 'OscIn 'oin)
    (decl 'OscMsg 'msg)
    (=> (call 'Std.atoi (call 'me.arg (int 0)))
        'oin.port)
    (call 'oin.addAddress (string "/quit,N"))
    (call 'oin.addAddress (string "/machine/status,N"))
    (call 'oin.addAddress (string "/machine/shreds,N"))
    (call 'oin.addAddress (string "/machine/add,s"))
    (call 'oin.addAddress (string "/machine/remove,i"))
    (call 'oin.addAddress (string "/machine/replace,is"))
    (<= chout (string "[engine] running") newline)
    (while (not-equal? 'running (int 0))
      (=> 'oin now)
      (while (not-equal? (call 'oin.recv 'msg) (int 0))
        (if (equal? 'msg.address (string "/quit"))
          (=> (int 0) 'running))
        (if (equal? 'msg.address (string "/machine/status"))
          (=> (call 'Machine.status) (decl 'int 'n))
          (<= chout (string "[engine] ") 'n newline))
        (if (equal? 'msg.address (string "/machine/shreds"))
          (@=> (call 'Machine.shreds) (array-decl 'int 'shreds))
          (<= chout (string "[engine] ["))
          (for (=> (int 0) (decl 'int 'i))
               (< 'i (call 'shreds.size))
               (inc 'i)
            (<= chout (array-ref 'shreds 'i))
            (if (< 'i (- (call 'shreds.size) (int 1)))
              (<= chout (string ", "))))
          (<= chout (string "]") newline))
        (if (equal? 'msg.address (string "/machine/add"))
          (<= chout
              (string "[engine] ")
              (call 'Machine.add (call 'msg.getString (int 0)))
              newline))
        (if (equal? 'msg.address (string "/machine/remove"))
          (<= chout
              (string "[engine] ")
              (call 'Machine.remove (call 'msg.getInt (int 0)))
              newline))
        (if (equal? 'msg.address (string "/machine/replace"))
          (<= chout
              (string "[engine] ")
              (call 'Machine.replace (call 'msg.getInt (int 0))
                                     (call 'msg.getString (int 1)))
              newline))))
    (<= chout (string "[engine] quit") newline)))

(struct engine-proc [p o i e oth eth shreds] #:mutable)

(define (thread-line-handler ch color line)
  (let ([m (regexp-match #rx"^\\[engine\\] (.*)$" line)])
    (if m
      (channel-put ch (last m))
      (printf (color "[chuck] ~a\n") line))))

(define (thread-output-handler ch o color)
  (thread
    (λ ()
      (let loop ()
        (define line
          (with-handlers ([exn:fail?
                           (λ (_) eof)])
            (read-line o)))
        (when (not (eof-object? line))
          (thread-line-handler ch color line)
          (loop))))))

(define (engine-full-start! chuck chuck-args chuck-port osc-port ch)
  (define c #f)
  (tempfile
    engine-code
    (λ (path)
      (define-values (p o i e)
        (apply subprocess (append (list #f #f #f
                                        chuck
                                        (format "--port ~a" chuck-port)
                                        (format "~a:~a" path osc-port))
                                  (or chuck-args '()))))
      (define oth (thread-output-handler ch o blue))
      (define eth (thread-output-handler ch e yellow))
      (engine-full-receive! ch) ; Wait for first message from chuck.
      (set! c (engine-proc p o i e oth eth (make-hash)))))
  c)

(define (engine-full-stop! c socket)
  (when c
    (close-output-port (engine-proc-i c))
    (close-input-port (engine-proc-o c))
    (close-input-port (engine-proc-e c))
    (when (not (udp-bound? socket)) (udp-close socket))
    (subprocess-kill (engine-proc-p c) #t)
    (kill-thread (engine-proc-oth c))
    (kill-thread (engine-proc-eth c)))
  (void))

(define (engine-full-receive! ch)
  ; NOTE: Max timeout of 1 second (hard-coded for now).
  (call-with-limits 1 #f (lambda () (channel-get ch))))

(define (engine-full-send! socket host osc-port route . args)
  (udp-send-to socket host osc-port (osc-element->bytes (osc-message route args)))
  (void))

(define (engine-full-register-shred! c id path)
  (hash-set! (engine-proc-shreds c) id path))

(define (engine-full-deregister-shred! c id)
  (hash-remove! (engine-proc-shreds c) id))
