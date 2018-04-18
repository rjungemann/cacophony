#lang racket

(require compatibility/defmacro)

(provide alm)

(define-for-syntax defs
  (let ([list-box (box '())]
        [define-in! (λ (b k v) (set-box! b (append (unbox b) (list (list k v)))))])
    ; Helpers.
    (define note-offset-lookup
      (hash "a" 0 "b" 2 "c" 3 "d" 5 "e" 7 "f" 8 "g" 10))
    (define accidental-offset-lookup
      (hash "-" -1 #f 0 "+" 1 "#" 1))
    (define (note-offset n a)
      (+ (hash-ref note-offset-lookup n) (hash-ref accidental-offset-lookup a)))
    (define (duration d dd)
      (if d (if dd (* (/ 1.0 d) 1.5) (/ 1.0 d)) d))
    ; Note definitions.
    (for ([n (list "a" "b" "c" "d" "e" "f" "g")])
      (for ([a (list "-" #f "+" "#")])
        (for ([d (list #f 1 2 4 8 16)])
          (for ([dd (list #f #t)])
            (define name (format "~a~a~a~a" n (if a a "") (if d d "") (if dd "." "")))
            (define-in! list-box (string->symbol name)
                                 `(list 'note ,(note-offset n a) ,(duration d dd)))))))
    ; Pause definitions.
    (for ([d (list #f 1 2 4 8 16)])
      (for ([dd (list #f #t)])
        (define name-p (format "p~a~a" (if d d "") (if dd "." "")))
        (define-in! list-box (string->symbol name-p) `(list 'rest ,(duration d dd)))
        (define name-r (format "r~a~a" (if d d "") (if dd "." "")))
        (define-in! list-box (string->symbol name-r) `(list 'rest ,(duration d dd)))))
    ; Length definitions.
    (for ([d (list #f 1 2 4 8 16)])
      (for ([dd (list #f #t)])
        (define name (format "l~a~a" (if d d "") (if dd "." "")))
        (define-in! list-box (string->symbol name) `(list 'length ,(duration d dd)))))
    ; Octave definitions.
    (for ([o (list -2 -1 0 1 2 3 4 5 6 7 8 9)])
      (define name (format "o~a" o))
      (define-in! list-box (string->symbol name) `(list 'octave ,o)))
    (define-in! list-box '< `(list 'octave-up))
    (define-in! list-box '> `(list 'octave-down))
    ; Tie definition.
    (define-in! list-box '~ `(list 'tie))
    ; Velocity definitions.
    (for ([v (in-range 1 128)])
      (define name (format "v~a" v))
      (define-in! list-box (string->symbol name) `(list 'velocity ,v)))
    ; Parse definition.
    ; TODO: Move to `foldl`.
    ; TODO: Second pass for ties.
    (define-in! list-box 'parse
      (λ events events
        (define current-time 0)
        (define current-octave 3)
        (define current-duration 0.25)
        (define current-velocity 64)
        (define event-list '())
        (define helpers
          (hash
            'note
            (λ (note-offset duration)
              (set! event-list
                    (append event-list
                            (list (list 'noteon
                                        (+ (* current-octave 12) note-offset)
                                        current-velocity
                                        current-time)
                                  (list 'noteoff
                                        (+ (* current-octave 12) note-offset)
                                        0
                                        (+ current-time (or duration current-duration))))))
              (set! current-time (+ current-time (or duration current-duration))))
            'rest
            (λ (duration)
              (set! current-time (+ current-time (or duration current-duration))))
            'length
            (λ (duration)
              (set! current-duration duration))
            'octave
            (λ (octave)
              (set! current-octave octave))
            'octave-up
            (λ ()
              (set! current-octave (+ current-octave 1)))
            'octave-down
            (λ ()
              (set! current-octave (- current-octave 1)))
            'tie
            (λ ()
              (set! event-list (append event-list (list (list 'tie)))))
            'velocity
            (λ (v)
              (set! current-velocity v))))
        (for ([event events])
          (apply (hash-ref helpers (car event) #f) (cdr event)))
        event-list
      )
    )
    (unbox list-box)))


(define-macro (alm . body)
  `(let* ,defs
    (parse ,@body)
  )
)

; If a tie is encountered,
; And the previous element is a noteoff,
; Delete the previous element.
; If a tie is encountered,
; And the previous element note is the same as the next element note,
; Delete the next element.
