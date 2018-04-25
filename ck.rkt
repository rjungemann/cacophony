#lang racket

(require compatibility/defmacro)

(provide ck)

; Generates the `ck` macro with the names of functions scoped inside of it.
(define-macro (def-scope name pairs)
  `(define-macro (,name . body)
    (define pairs (quote ,pairs))
    `(let* (,@pairs)
      (do ,@body))))

(def-scope ck
  (
   ;; Definitions that other definitions depend on
   [exprs
    (λ stmts stmts
      (string-join (map (λ (stmt) (format "~a" stmt)) stmts) ", "))]
   [do
    (λ stmts stmts
      (string-join (map (λ (stmt) (format "~a;" stmt)) stmts) "\n"))]
   [call
    (λ (fn . args)
     (format "~a(~a)" fn (apply exprs args)))]
   ;; Types and values
   [int
    (λ (n)
      (format "~a" n))]
   [float
    (λ (n)
      (format "~a" n))]
   [string
    (λ (s)
      (format "~s" s))]
   [dur
    (λ (n t)
      (format "~a::~a" n t))]
   [complex
    (λ (r i)
      (format "#(~a, ~a)" r i))]
   [polar
    (λ (m p)
      (format "#(~a, ~a)" m p))]
   [array
    (λ args args
      (format "[~a]" (apply exprs args)))]
   [array-ref
    (λ (name key)
      (format "~a[~a]" name key))]
   [now
    'now]
   [pi
    'pi]
   [void
    'void]
   [true
    'true]
   [false
    'false]
   [null
    'null]
   [me
    'me]
   [samp
    'samp]
   [ms
    'ms]
   [second
    'second]
   [minute
    'minute]
   [hour
    'hour]
   [day
    'day]
   [week
    'week]
   [chout
    'chout]
   [cherr
    'cherr]
   [newline
    (string->symbol "IO.newline()")]
   [adc
    'adc]
   [dac
    'dac]
   [blackhole
    'blackhole]
   [inlet
    'inlet]
   [outlet
    'outlet]
   [class
    (λ (name extends . body)
      (define baked-extends (if extends (format " extends ~a" extends) ""))
      (define baked-body (apply do body))
      (format "class ~a~a {\n~a\n}" name baked-extends baked-body))]
   [public-class
    (λ (name extends . body)
      (define baked-extends (if extends (format " extends ~a" extends) ""))
      (define baked-body (apply do body))
      (format "public class ~a~a {\n~a\n}" name baked-extends baked-body))]
   [this
    'this]
   ;; Basics
   [inspect
    (λ args args
      (format "<<< ~a >>>" (apply exprs args)))]
   [oper
    (λ (op . args)
      (format "(~a)" (string-join (map (λ (n) (format "~a" n)) args) (format " ~a " op))))]
   [decl
    (λ (t name)
      (format "~a ~a" t name))]
   [static-decl
    (λ (t name)
      (format "static ~a ~a" t name))]
   [@-decl
    (λ (t name)
      (format "~a @ ~a" t name))]
   [static-@-decl
    (λ (t name)
      (format "static ~a @ ~a" t name))]
   [array-decl
    (λ (t name)
      (format "~a ~a[]" t name))]
   [array-static-decl
    (λ (t name)
      (format "static ~a ~a[]" t name))]
   [array-@-decl
    (λ (t name)
      (format "~a @ ~a[]" t name))]
   [array-static-@-decl
    (λ (t name)
      (format "static ~a @ ~a[]" t name))]
   [fun
    (λ (t name params . body)
      (format "fun ~a ~a (~a) {\n~a\n}" t name (apply exprs params) (apply do body)))]
   [static-fun
    (λ (t name params . body)
      (format "fun static ~a ~a (~a) {\n~a\n}" t name (apply exprs params) (apply do body)))]
   [return
     (λ (v)
       (format "return ~a" v))]
   [spork
     (λ (v [ref #f])
       (if ref
         (format "spork ~~ ~a @ ~a" v ref)
         (format "spork ~~ ~a" v)))]
   ;; Operators
   [=>
    (λ args args
      (apply oper (append (list '=>) args)))]
   [=<
    (λ args args
      (apply oper (append (list '=<) args)))]
   [=^
    (λ args args
      (apply oper (append (list '=^) args)))]
   [@=>
    (λ args args
      (apply oper (append (list '@=>) args)))]
   [+
    (λ args args
      (apply oper (append (list '+) args)))]
   [-
    (λ args args
      (apply oper (append (list '-) args)))]
   [*
    (λ args args
      (apply oper (append (list '*) args)))]
   [/
    (λ args args
      (apply oper (append (list '/) args)))]
   [%
    (λ args args
      (apply oper (append (list '%) args)))]
   [+=>
    (λ args args
      (apply oper (append (list '+=>) args)))]
   [-=>
    (λ args args
      (apply oper (append (list '-=>) args)))]
   [*=>
    (λ args args
      (apply oper (append (list '*=>) args)))]
   [/=>
    (λ args args
      (apply oper (append (list '/=>) args)))]
   [%=>
    (λ args args
      (apply oper (append (list '%=>) args)))]
   [cast
    (λ args args
      (apply oper (append (list '$) args)))]
   [and
    (λ args args
      (apply oper (append (list '&&) args)))]
   [or
    (λ args args
      (apply oper (append (list '||) args)))]
   [equal?
    (λ args args
      (apply oper (append (list '==) args)))]
   [not-equal?
    (λ args args
      (apply oper (append (list '!=) args)))]
   [>
    (λ args args
      (apply oper (append (list '>) args)))]
   [>=
    (λ args args
      (apply oper (append (list '>=) args)))]
   [<
    (λ args args
      (apply oper (append (list '<) args)))]
   [<=
    (λ args args
      (apply oper (append (list '<=) args)))]
   [bit-shift-r
    (λ args args
      (apply oper (append (list '>>) args)))]
   [bit-shift-l
    (λ args args
      (apply oper (append (list '<<) args)))]
   [bit-and
    (λ args args
      (apply oper (append (list '&) args)))]
   [bit-or
    (λ args args
      (apply oper (append (list (string->symbol "|")) args)))]
   [bit-xor
    (λ args args
      (apply oper (append (list '^) args)))]
   [inc
    (λ (arg)
      (format "~a++" arg))]
   [pre-inc
    (λ (arg)
      (format "++~a" arg))]
   [dec
    (λ (arg)
      (format "~a--" arg))]
   [pre-dec
    (λ (arg)
      (format "--~a" arg))]
   [not
    (λ (arg)
      (format "!~a" arg))]
   [positive
    (λ (arg)
      (format "+~a" arg))]
   [negate
    (λ (arg)
      (format "-~a" arg))]
   [new
    (λ (arg)
      (format "new ~a" arg))]
   [if
    (λ (expr1 . stmts)
      (format "if (~a) {\n~a\n}" expr1 (apply do stmts)))]
   [while
    (λ (expr1 . stmts)
      (format "while (~a) {\n~a\n}" expr1 (apply do stmts)))]
   [repeat
    (λ (expr1 . stmts)
      (format "repeat (~a) {\n~a\n}" expr1 (apply do stmts)))]
   [until
    (λ (expr1 . stmts)
      (format "until (~a) {\n~a\n}" expr1 (apply do stmts)))]
   [do-while
    (λ (expr1 . stmts)
      (format "do {\n~a\n} while (~a)" expr1 (apply do stmts)))]
   [do-until
    (λ (expr1 . stmts)
      (format "do {\n~a\n} until (~a)" expr1 (apply do stmts)))]
   [for
    (λ (expr1 expr2 expr3 . stmts)
      (format "for (~a; ~a; ~a) {\n~a\n}" expr1 expr2 expr3 (apply do stmts)))]
   [break
    'break]
   [continue
    'continue]))
