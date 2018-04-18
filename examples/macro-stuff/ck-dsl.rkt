#lang racket

(require compatibility/defmacro)

(provide ck)

; Generates the `ck` macro with the names of functions scoped inside of it. 
(define-macro (def-scope name pairs)
  `(define-macro (,name . body)
    (define pairs (quote ,pairs))
    `(let (,@pairs)
      (do ,@body))))

(def-scope ck
  ([do
    (λ stmts stmts
      (string-join stmts "; "))]
   [schedule
    (λ ()
      "schedule")]
   [foo
    (λ (a)
      (format "foo ~a" a))]))
