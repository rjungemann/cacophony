#lang racket

(require compatibility/defmacro
         threading)

(provide alm
         alm-parse)

(define (alm-note? s)
  (and (string? s) (regexp-match #rx"^[abcdefg][-+#]?[0-9]*\\.?$" s)))

(define note-offsets
  (hash "a" 0 "b" 2 "c" 3 "d" 5 "e" 7 "f" 8 "g" 10))

(define (alm-process-note s octave-box duration-box velocity-box)
  (define flat? (regexp-match #rx"\\-" s))
  (define sharp? (regexp-match #rx"[+#]" s))
  (define basic-note (first (regexp-match #rx"^[a-g]" s)))
  (define basic-note-offset (hash-ref note-offsets basic-note))
  (define note-offset (cond [flat? (- basic-note-offset 1)]
                            [sharp? (+ basic-note-offset 1)]
                            [else basic-note-offset]))
  (define note (+ (* (unbox octave-box) 12) note-offset))
  (define basic-duration-match (regexp-match #rx"[0-9]+" s))
  (define basic-duration (if basic-duration-match
                           (* (/ 1.0 (string->number (first basic-duration-match))) 4)
                           (unbox duration-box)))
  (define dotted? (regexp-match #rx"\\.$" s))
  (define duration (if dotted? (* basic-duration 1.5) basic-duration))
  (list 'note note (unbox velocity-box) duration))

(define (alm-rest? s)
  (and (string? s) (regexp-match #rx"^[pr][0-9]*$" s)))

(define (alm-process-rest s duration-box)
  (define basic-duration-match (regexp-match #rx"[0-9]+" s))
  (define basic-duration (if basic-duration-match
                            (* (/ 1.0 (string->number (first basic-duration-match))) 4)
                            (unbox duration-box)))
  (define dotted? (regexp-match #rx"\\.$" s))
  (define duration (if dotted? (* basic-duration 1.5) basic-duration))
  (list 'rest duration))

(define (alm-duration? s)
  (and (string? s) (regexp-match #rx"^l[0-9]+$" s)))

(define (alm-process-duration s duration-box)
  (define basic-duration-match (regexp-match #rx"[0-9]+" s))
  (set-box! duration-box
            (* (/ 1.0 (string->number (first basic-duration-match))) 4))
  #f)

(define (alm-velocity? s)
  (and (string? s) (regexp-match #rx"^v[0-9]+$" s)))

(define (alm-process-velocity s velocity-box)
  (define basic-velocity-match (regexp-match #rx"[0-9]+" s))
  (set-box! velocity-box (string->number (first basic-velocity-match)))
  #f)

(define (alm-octave-down? s)
  (equal? s ">"))

(define (alm-process-octave-down octave-box)
  (set-box! octave-box (- (unbox octave-box) 1))
  #f)

(define (alm-octave-up? s)
  (equal? s "<"))

(define (alm-process-octave-up octave-box)
  (set-box! octave-box (+ (unbox octave-box) 1))
  #f)

(define (alm-octave? s)
  (and (string? s) (regexp-match #rx"^o[0-9]+$" s)))

(define (alm-process-octave s octave-box)
  (define basic-octave-match (regexp-match #rx"[0-9]+" s))
  (set-box! octave-box
            (string->number (first basic-octave-match)))
  #f)

(define (alm-tie? s)
  (equal? s "~"))

(define (alm-identify n octave-box duration-box velocity-box)
  (define s (and (symbol? n) (symbol->string n)))
  (cond [(alm-note? s)
         (alm-process-note s octave-box duration-box velocity-box)]
        [(alm-rest? s)
         (alm-process-rest s duration-box)]
        [(alm-duration? s)
         (alm-process-duration s duration-box)]
        [(alm-velocity? s)
         (alm-process-velocity s velocity-box)]
        [(alm-tie? s)
         '(tie)]
        [(alm-octave-down? s)
         (alm-process-octave-down octave-box)]
        [(alm-octave-up? s)
         (alm-process-octave-up octave-box)]
        [(alm-octave? s)
         (alm-process-octave s octave-box)]
        [else
         n]))

(define (filter-falses elements)
  (filter (λ (n) (not (false? n))) elements))

(define (alm-identify-elements elements octave-box duration-box velocity-box)
  (map (λ (n) (alm-identify n octave-box duration-box velocity-box)) elements))

(define (alm-with-absolute-times elements time-box)
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
       elements))

(define (alm-validate elements)
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
  elements)

(define (alm-preprocess-ties elements)
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
  elements)

(define (alm-strip-ties elements)
  (filter (λ (n) (not (equal? n '(tie)))) elements))

(define (alm-generate-note-pairs elements)
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
    elements))

(define (alm-fix-absolute-times elements)
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
    elements))

(define (alm-parse body)
  (define duration-box (box 1.0))
  (define octave-box (box 3))
  (define velocity-box (box 64))
  (define time-box (box 0))
  (~> body
      (alm-identify-elements octave-box duration-box velocity-box)
      (filter-falses)
      (alm-with-absolute-times time-box)
      (alm-validate)
      (alm-preprocess-ties)
      (alm-strip-ties)
      (alm-generate-note-pairs)
      (filter-falses)
      (alm-fix-absolute-times)))

(define-macro (alm . body)
  `(alm-parse (quote ,body)))
