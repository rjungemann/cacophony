#lang racket

(require compatibility/defmacro)

(provide ck)

#| ; TODO: Make this hygenic! |#
#| (define-macro (ck . body) |#
#|   (let ([n (make-base-namespace)]) |#
#|     (parameterize ([current-namespace n]) |#
#|       (namespace-require "ck-dsl.rkt")) |#
#|     `(eval '(do ,@body) ,n))) |#

; TODO: Make this hygenic!
(define-macro (ck body)
  (let ([n (make-base-namespace)])
    (parameterize ([current-namespace n])
      (namespace-require "ck-dsl.rkt"))
    `(eval ,body ,n)))

#| (define n 1) |#
#| (ck `(do (<<<>>> (int 0) (int ,n)))) |#
