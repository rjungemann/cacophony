#lang racket

(provide socket-start!
         socket-eval!)

(define (socket-start! port ich och)
  (define listener (tcp-listen port 5 #t))
  (let loop ()
    (define-values (i o) (tcp-accept listener))
    (let loop ()
      (define line (read-line i))
      (and (not (eof-object? line))
           (begin
             (channel-put ich line)
             (let ([in (channel-get och)])
               (displayln in o)
               (flush-output o)
               (loop)))))
    (close-input-port i)
    (close-output-port o)
    (loop)))

(define (socket-eval! ich och)
  (define line (channel-try-get ich))
  (and line (not (equal? line eof))
       (begin
         (channel-put
           och
           (with-handlers ([(lambda (v) #t) (lambda (ex) ex)])
             (eval (call-with-input-string line read)))))))
