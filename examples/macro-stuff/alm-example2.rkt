#lang racket

; yes | raco pkg install --skip-installed zippers
(require zippers)

(define a '((noteon 36 64 0) (noteoff 36 0 0.375) (noteon 37 64 0.375) (noteoff 37 0 0.875) (noteon 37 64 0.875) (noteoff 37 0 1.375) (noteon 35 64 1.375) (noteoff 35 0 1.875) (tie) (noteon 39 64 1.875) (noteoff 39 0 2.125) (noteon 40 64 2.125) (noteoff 40 0 2.875)))
(define b '((noteon 36 64 0) (noteoff 36 0 0.375) (noteon 37 64 0.375) (noteoff 37 0 0.875) (noteon 37 64 0.875) (noteoff 37 0 1.375) (noteon 35 64 1.375) (noteoff 35 0 1.875) (tie) (noteon 35 64 1.875) (noteoff 35 0 2.125) (noteon 39 64 2.125) (noteoff 39 0 2.375) (noteon 40 64 2.375) (noteoff 40 0 3.125)))

(define (rem-n l n)
  (append (take l n) (drop l (+ n 1))))

(define (handle-ties events)
  (define z (down/list-first (zip events)))
  (let loop ()
    (displayln (zipper-focus z))
    (if (and (not (empty? (zipper-focus z)))
             (equal? (first (zipper-focus z)) 'tie))
      (begin
        (set! z (left/list z))
        (let ([p-n (list-ref (zipper-focus z) 1)])
          (set! z (edit (λ (v) '()) z))
          (set! z (right/list z))
          (set! z (edit (λ (v) '()) z))
          (set! z (right/list z))
          (when (and (equal? (first (zipper-focus z)) 'noteon)
                     (equal? (list-ref (zipper-focus z) 1) p-n))
            (edit (λ (v) '()) z)
            (set! z (left/list z)))))
      (set! z (right/list z)))
    (if (can-move? right/list z)
      (loop)
      (filter (λ (n) (not (empty? n))) (zipper-focus (up z))))))

#| (define (handle-ties events) |#
#|   (let loop ([es events] |#
#|              [i 0]) |#
#|     (cond |#
#|       [(and (= i (length es))) |#
#|        ; Break |#
#|        es |#
#|       ] |#
#|       [(and (equal? (list 'tie) (list-ref es i)) |#
#|             (= i (- (length es) 1))) |#
#|        ; Delete current element |#
#|        (loop (rem-n es i) i) |#
#|       ] |#
#|       [(= i (- (length es) 1)) |#
#|        ; Do nothing and increment i |#
#|        (loop es (+ i 1)) |#
#|       ] |#
#|       [(and (equal? (list 'tie) (list-ref es i)) |#
#|             (= i 0)) |#
#|        ; Delete current element |#
#|        (loop (rem-n es i) i) |#
#|       ] |#
#|       [(and (equal? (list 'tie) (list-ref es i)) |#
#|             (equal? (first (list-ref es (- i 1))) 'noteoff) |#
#|             (equal? (first (list-ref es (+ i 1))) 'noteon)) |#
#|        ; Delete previous, current, and next element and decrement i |#
#|        (let* ([es-1 (rem-n es (- i 1))] |#
#|               [es-2 (rem-n es-1 i)] |#
#|               [es-3 (rem-n es-2 (+ i 1))]) |#
#|          (loop es-3 (- i 1))) |#
#|       ] |#
#|       [(and (equal? (list 'tie) (list-ref es i)) |#
#|             (equal? (first (list-ref es (- i 1))) 'noteoff)) |#
#|        ; Delete previous and current element and decrement i |#
#|        (let* ([es-1 (rem-n es (- i 1))] |#
#|               [es-2 (rem-n es-1 i)]) |#
#|          (loop es-2 (- i 1))) |#
#|       ] |#
#|       [(and (equal? (list 'tie) (list-ref es i)) |#
#|             (equal? (first (list-ref es (+ i 1))) 'noteon)) |#
#|        ; Delete current and next element |#
#|        (let* ([es-1 (rem-n es i)] |#
#|               [es-2 (rem-n es-1 (+ i 1))]) |#
#|          (loop es-2 i)) |#
#|       ] |#
#|       [else |#
#|        ; Skip element |#
#|        (loop es (+ i 1)) |#
#|       ] |#
#|     ) |#
#|   ) |#
#| ) |#

(printf "handle-ties ~a\n" (handle-ties a))
(printf "handle-ties ~a\n" (handle-ties b))
