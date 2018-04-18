#lang racket

(define-namespace-anchor anc)
(define ns (namespace-anchor->namespace anc))

(define notes (list "a" "b" "c" "d" "e" "f" "g"))
(define accidentals (list #f "-" "+"))
(define durations (list #f "1." "1" "2." "2" "4." "4" "8." "8" "16." "16" "32"))

(for ([n notes])
  (for ([a accidentals])
    (for ([d durations])
      (define name (format "~a~a~a" n (if a a "") (if d d "")))
      (eval (list 'define (string->symbol name) (list 'list '(quote note) n a d)) ns))))

(for ([d durations])
  (when d
    (define name (format "l~a" (if d d "")))
    (eval (list 'define (string->symbol name) (list 'list '(quote duration) d)) ns)))

(define > (list 'octave-down))
(define < (list 'octave-up))
(define ~ (list 'tie))

(eval '(displayln a4.) ns)
