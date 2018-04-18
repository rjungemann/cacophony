#lang racket

(require compatibility/defmacro
         zippers)

(provide alm
         (all-from-out zippers))

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
    (unbox list-box)))

(define-for-syntax define-parse
  `(define (parse . events)
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
    event-list))

(define-for-syntax define-handle-ties
  `(define (handle-ties events)
    (define (rem-n l n)
      (append (take l n) (drop l (+ n 1))))
    (define z (down/list-first (zip events)))
    (let loop ()
      ; If a tie is encountered,
      (if (and (not (empty? (zipper-focus z)))
               (equal? (first (zipper-focus z)) 'tie))
        (begin
          ; Examine the previous element.
          (set! z (left/list z))
          (let ([p-n (list-ref (zipper-focus z) 1)])
            ; Empty the previous element.
            (set! z (edit (λ (v) '()) z))
            ; Examine the current element.
            (set! z (right/list z))
            ; Empty the current element
            (set! z (edit (λ (v) '()) z))
            ; If the next element is a noteon, and is the same note as the
            ; previous element, empty it.
            (if (can-move? right/list z)
              (set! z (right/list z))
              (when (and (equal? (first (zipper-focus z)) 'noteon)
                         (equal? (list-ref (zipper-focus z) 1) p-n))
                (edit (λ (v) '()) z)
                (set! z (left/list z))))))
        ; Set the current element to the next element.
        (set! z (right/list z)))
      ; Keep looping while we can move rightward.
      (if (can-move? right/list z)
        (loop)
        (begin
          ; If there's a tie at the end, replace it with an empty.
          (when (and (not (empty? (zipper-focus z)))
                   (equal? (first (zipper-focus z)) 'tie))
            (set! z (left/list z))
            (set! z (edit (λ (v) '()) z))
            (set! z (right/list z))
            (set! z (edit (λ (v) '()) z)))
          ; Strip the empties.
          (filter (λ (n) (not (empty? n))) (zipper-focus (up z))))))))

; TODO: Add relative positioning pass.
; TODO: Slow. Consider simplifying by parsing manually instead of defining.
(define-macro (alm . body)
  `(let* ,defs
    ,define-parse
    ,define-handle-ties
    (handle-ties (parse ,@body))))
