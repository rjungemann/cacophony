#lang racket

(require compatibility/defmacro
         "ck-dsl.rkt")

(define a 1)
(ck
  (schedule)
  (foo a))
