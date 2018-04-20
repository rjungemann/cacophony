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
    (unbox list-box)))

(define-for-syntax alm-with-absolute-times
  `(λ (elements time-box)
    (map (λ (n)
           (define kind (first n))
           (if (or (equal? kind 'note) (equal? kind 'rest))
               (let* ([t (last n)]
                      [start-time (unbox time-box)]
                      [end-time (+ start-time t)])
                 (define new-n (append n (list (list start-time end-time))))
                 (set-box! time-box end-time)
                 new-n)
               n))
         elements)))

(define-for-syntax alm-validate
  `(λ (elements)
    (for ([current elements]
          [i (in-naturals)])
      (let ([previous (and (> i 0) (list-ref elements (- i 1)))]
            [next (and (< i (- (length elements) 1)) (list-ref elements (+ i 1)))])
        (when (and (equal? current '(tie)) (equal? next '(tie)))
          (error "Can't have two ties in a row!"))
        (when (and (not (and (list? previous) (equal? (first previous) 'note)))
                   (equal? current '(tie))
                   (not (and (list? next) (equal? (first next) 'note))))
          (error "A tie must have a note before and after!"))))
    elements))

(define-for-syntax alm-preprocess-ties
  `(λ (elements)
    (for ([element elements]
          [i (in-naturals)])
      (let* ([has-prior? (> i 1)]
             [has-previous? (> i 0)]
             [has-next? (< i (- (length elements) 1))]
             [has-after? (< i (- (length elements) 2))]
             [previous-element (and has-previous?  (list-ref elements (- i 1)))]
             [next-element (and has-next?  (list-ref elements (+ i 1)))]
             [previous-element-tie? (and previous-element (equal? previous-element '(tie)))]
             [next-element-tie? (and next-element (equal? next-element '(tie)))]
             [prior-element (and has-prior? previous-element-tie? (list-ref elements (- i 2)))]
             [after-element (and has-after? next-element-tie?  (list-ref elements (+ i 2)))])
      (let ([prior-note? (and prior-element (equal? 'note (first prior-element)))]
            [after-note? (and after-element (equal? 'note (first after-element)))])
        (when (equal? (first element) 'note)
          (let* ([new-prior-element (and prior-note? (take prior-element 5))]
                 [new-after-element (and after-note? (take after-element 5))]
                 [linked-elements (list new-prior-element new-after-element)]
                 [new-element (append element (list linked-elements))])
            (set! elements (list-set elements i new-element)))))))
    elements))

(define-for-syntax alm-strip-ties
  `(λ (elements)
    (filter (λ (n) (not (equal? n '(tie)))) elements)))

(define-for-syntax alm-generate-note-pairs
  `(λ (elements)
    (append-map
      (λ (n)
        (if (equal? (first n) 'note)
            (let* ([new-note (take n 5)]
                   [linked-elements (last n)]
                   [prior-element (first linked-elements)]
                   [after-element (last linked-elements)]
                   [prior-element-n (and prior-element (list-ref prior-element 1))]
                   [new-note-n (list-ref n 1)])
              (list (and (not (equal? prior-element-n new-note-n))
                         (append (list 'noteon) (drop new-note 1)))
                    (and (not after-element)
                         (append (list 'noteoff (list-ref new-note 1) 0)
                                 (drop new-note 3)))))
            (list n)))
      elements)))

(define-for-syntax alm-filter-falses
  `(λ (elements)
    (filter (λ (n) (not (false? n))) elements)))

(define-for-syntax alm-fix-absolute-times
  `(λ (elements)
    (map
      (λ (n)
        (cond
         [(equal? (first n) 'noteon)
          (list-set n (- (length n) 1) (first (last n)))]
         [(equal? (first n) 'noteoff)
          (list-set n (- (length n) 1) (last (last n)))]
         [(equal? (first n) 'rest)
          (list-set n (- (length n) 1) (first (last n)))]
         [else
          n]))
      elements)))

(define-for-syntax alm-parse
  `(define (alm-parse body)
    (define duration-box (box 1.0))
    (define octave-box (box 3))
    (define velocity-box (box 64))
    (define time-box (box 0))
    (let* ([result-1 (,alm-with-absolute-times body time-box)]
           #| [result-2 (,alm-validate result-1)] |#
           #| [result-3 (,alm-preprocess-ties result-2)] |#
           #| [result-4 (,alm-strip-ties result-3)] |#
           #| [result-5 (,alm-generate-note-pairs result-4)] |#
           #| [result-6 (,alm-filter-falses result-5)] |#
           #| [result-7 (,alm-fix-absolute-times result-6)] |#
           )
      result-1)))

; TODO: Add relative positioning pass.
; TODO: Slow. Consider simplifying by parsing manually instead of defining.
(define-macro (alm . body)
  `(let* ,defs
    ,alm-parse
    (alm-parse (list ,@body))))
