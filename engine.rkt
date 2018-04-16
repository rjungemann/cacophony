#lang racket

(require osc
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
  (ck-do
    (ck-=> (ck-int 1) (ck-decl 'int 'running))
    (ck-decl 'OscIn 'oin)
    (ck-decl 'OscMsg 'msg)
    (ck-=> (ck-ref-call 'Std 'atoi (ck-ref-call 'me 'arg (ck-int 0)))
           (ck-ref 'oin 'port))
    (ck-ref-call 'oin 'addAddress (ck-string "/quit,N"))
    (ck-ref-call 'oin 'addAddress (ck-string "/machine/status,N"))
    (ck-ref-call 'oin 'addAddress (ck-string "/machine/shreds,N"))
    (ck-ref-call 'oin 'addAddress (ck-string "/machine/add,s"))
    (ck-ref-call 'oin 'addAddress (ck-string "/machine/remove,i"))
    (ck-ref-call 'oin 'addAddress (ck-string "/machine/replace,is"))
    (ck-<= ck-chout (ck-string "[engine] running") ck-newline)
    (ck-while (ck-not-equal? 'running (ck-int 0))
      (ck-=> 'oin ck-now)
      (ck-while (ck-not-equal? (ck-ref-call 'oin 'recv 'msg) (ck-int 0))
        (ck-if (ck-equal? (ck-ref 'msg 'address) (ck-string "/quit"))
          (ck-=> (ck-int 0) 'running))
        (ck-if (ck-equal? (ck-ref 'msg 'address) (ck-string "/machine/status"))
          (ck-=> (ck-ref-call 'Machine 'status) (ck-decl 'int 'n))
          (ck-<= ck-chout (ck-string "[engine] ") 'n ck-newline))
        (ck-if (ck-equal? (ck-ref 'msg 'address) (ck-string "/machine/shreds"))
          (ck-@=> (ck-ref-call 'Machine 'shreds) (ck-array-decl 'int 'shreds))
          (ck-<= ck-chout (ck-string "[engine] ["))
          (ck-for (ck-=> (ck-int 0) (ck-decl 'int 'i))
                  (ck-< 'i (ck-ref-call 'shreds 'size))
                  (ck-inc 'i)
            (ck-<= ck-chout (ck-array-ref 'shreds 'i))
            (ck-if (ck-< 'i (ck-- (ck-ref-call 'shreds 'size) (ck-int 1)))
              (ck-<= ck-chout (ck-string ", "))))
          (ck-<= ck-chout (ck-string "]") ck-newline))
        (ck-if (ck-equal? (ck-ref 'msg 'address) (ck-string "/machine/add"))
          (ck-<= ck-chout
                 (ck-string "[engine] ")
                 (ck-ref-call 'Machine 'add (ck-ref-call 'msg 'getString (ck-int 0)))
                 ck-newline))
        (ck-if (ck-equal? (ck-ref 'msg 'address) (ck-string "/machine/remove"))
          (ck-<= ck-chout
                 (ck-string "[engine] ")
                 (ck-ref-call 'Machine 'remove (ck-ref-call 'msg 'getInt (ck-int 0)))
                 ck-newline))
        (ck-if (ck-equal? (ck-ref 'msg 'address) (ck-string "/machine/replace"))
          (ck-<= ck-chout
                 (ck-string "[engine] ")
                 (ck-ref-call 'Machine 'replace (ck-ref-call 'msg 'getInt (ck-int 0))
                                                (ck-ref-call 'msg 'getString (ck-int 1)))
                 ck-newline))))
    (ck-<= ck-chout (ck-string "[engine] quit") ck-newline)))

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
          (begin
            (thread-line-handler ch color line)
            (loop)))))))

(define (engine-full-start! chuck chuck-port osc-port ch)
  (tempfile
    engine-code
    (λ (path)
      (define-values (p o i e)
        (apply subprocess (list #f #f #f
                                chuck
                                (format "--port ~a" chuck-port)
                                (format "~a:~a" path osc-port))))
      (define oth (thread-output-handler ch o blue))
      (define eth (thread-output-handler ch e yellow))
      (engine-full-receive! ch) ; Wait for first message from chuck.
      (engine-proc p o i e oth eth (make-hash)))))

(define (engine-full-stop! c socket)
  (when c
    (begin
      (close-output-port (engine-proc-i c))
      (close-input-port (engine-proc-o c))
      (close-input-port (engine-proc-e c))
      (when (not (udp-bound? socket)) (udp-close socket))
      (subprocess-kill (engine-proc-p c) #t)
      (kill-thread (engine-proc-oth c))
      (kill-thread (engine-proc-eth c))))
  (void))

(define (engine-full-receive! ch)
  (channel-get ch))

(define (engine-full-send! socket host osc-port route . args)
  (udp-send-to socket host osc-port (osc-element->bytes (osc-message route args)))
  (void))

(define (engine-full-register-shred! c id path)
  (hash-set! (engine-proc-shreds c) id path))

(define (engine-full-deregister-shred! c id)
  (hash-remove! (engine-proc-shreds c) id))
