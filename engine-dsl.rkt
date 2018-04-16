#lang racket

(require "utils.rkt"
         "engine.rkt")

(provide current-engine-chuck
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
         engine-unshredule)

(define current-engine-chuck
  (make-parameter "/usr/local/bin/chuck"))

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
  (define c (current-engine-proc))
  (when (not c)
    (set-box! (current-engine-proc) (engine-full-start! (current-engine-chuck)
                                                        (current-engine-chuck-port)
                                                        (current-engine-osc-port)
                                                        (current-engine-channel)))))

(define (engine-stop!)
  (define c (current-engine-proc))
  (when c
    (engine-full-stop! (current-engine-proc) (current-engine-socket))
    (set-box! (current-engine-proc) #f))
  (void))

(define (engine-receive!)
  (engine-full-receive! (current-engine-channel)))

(define (engine-send! route . args)
  (engine-full-send! (append (list (current-engine-socket)
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
  (if (file-exists? (bytes->string/utf-8 path))
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
        (register-shred! id (string-join (append (list path) args) ":"))
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
