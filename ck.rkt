#lang racket

(require "utils.rkt")

(provide (all-defined-out))

;; Types and values
(define (ck-int n) (format "~a" n))
(define (ck-float n) (format "~a" n))
(define (ck-string s) (format "~s" s))
(define (ck-dur n t) (format "~a::~a" n t))
(define (ck-complex r i) (format "#(~a, ~a)" r i))
(define (ck-polar m p) (format "%(~a, ~a)" m p))
(define (ck-array . args) (format "[~a]" (apply ck-exprs args)))
(define (ck-array-ref name key) (format "~a[~a]" name key))
(define ck-now 'now)
(define ck-pi 'pi)
(define ck-void 'void)
(define ck-true 'true)
(define ck-false 'false)
(define ck-null 'null)
(define ck-me 'me)
(define ck-samp 'samp)
(define ck-ms 'ms)
(define ck-second 'second)
(define ck-minute 'minute)
(define ck-hour 'hour)
(define ck-day 'day)
(define ck-week 'week)
(define ck-chout 'chout)
(define ck-cherr 'cherr)
(define ck-newline (string->symbol "IO.newline()"))
(define ck-adc 'adc)
(define ck-dac 'dac)
(define ck-blackhole 'blackhole)

;; Classes
(define (ck-class name attr extends . body)
  (define baked-attr (if attr (format "~a " attr) ""))
  (define baked-extends (if extends (format " extends ~a" extends) ""))
  (define baked-body (apply ck-do body))
  (format "~a~a ~a~a {\n~a\n}" baked-attr "class" name baked-extends baked-body))
(define (ck-ref name key) (format "~a.~a" name key))
(define (ck-ref-call name fn . args) (format "~a.~a" name (apply ck-call (append (list fn) args))))
(define ck-this 'this)

;; Basics
(define (ck-do . stmts)
  (string-join (map (λ (stmt) (format "~a;" stmt)) stmts) "\n"))
(define (ck-exprs . stmts)
  (string-join (map (λ (stmt) (format "~a" stmt)) stmts) ", "))
(define (ck-<<<>>> . args)
  (format "<<< ~a >>>" (apply ck-exprs args)))
(define (ck-oper op . args)
  (string-join (map (λ (n) (format "~a" n)) args) (format " ~a " op)))
(define (ck-decl t name) (format "~a ~a" t name))
(define (ck-static-decl t name) (format "static ~a ~a" t name))
(define (ck-array-decl t name) (format "~a ~a[]" t name))
(define (ck-call fn . args)
  (format "~a(~a)" fn (apply ck-exprs args)))
(define (ck-fun t name params . body)
  (format "fun ~a ~a (~a) {\n~a\n}" t name (apply ck-exprs params) (apply ck-do body)))
(define (ck-static-fun t name params . body)
  (format "fun static ~a ~a (~a) {\n~a\n}" t name (apply ck-exprs params) (apply ck-do body)))
(define (ck-return v) (format "return ~a" v))
(define (ck-spork v [ref #f])
  (if ref
    (format "spork ~~ ~a @ ~a" v ref)
    (format "spork ~~ ~a" v)))

;; Operators
(define (ck-=> . args) (apply ck-oper (append (list '=>) args)))
(define (ck-=< . args) (apply ck-oper (append (list '=>) args)))
(define (ck-=^ . args) (apply ck-oper (append (list '@=>) args)))
(define (ck-@=> . args) (apply ck-oper (append (list '@=>) args)))
(define (ck-+ . args) (apply ck-oper (append (list '+) args)))
(define (ck-- . args) (apply ck-oper (append (list '-) args)))
(define (ck-* . args) (apply ck-oper (append (list '*) args)))
(define (ck-/ . args) (apply ck-oper (append (list '/) args)))
(define (ck-% . args) (apply ck-oper (append (list '%) args)))
(define (ck-+=> . args) (apply ck-oper (append (list '+=>) args)))
(define (ck--=> . args) (apply ck-oper (append (list '-=>) args)))
(define (ck-*=> . args) (apply ck-oper (append (list '*=>) args)))
(define (ck-/=> . args) (apply ck-oper (append (list '/=>) args)))
(define (ck-%=> . args) (apply ck-oper (append (list '%=>) args)))
(define (ck-cast . args) (apply ck-oper (append (list '$) args)))
(define (ck-and . args) (apply ck-oper (append (list '&&) args)))
(define (ck-or . args) (apply ck-oper (append (list '||) args)))
(define (ck-equal? . args) (apply ck-oper (append (list '==) args)))
(define (ck-not-equal? . args) (apply ck-oper (append (list '!=) args)))
(define (ck-> . args) (apply ck-oper (append (list '>) args)))
(define (ck->= . args) (apply ck-oper (append (list '>=) args)))
(define (ck-< . args) (apply ck-oper (append (list '<) args)))
(define (ck-<= . args) (apply ck-oper (append (list '<=) args)))
(define (ck-bit-shift-r . args) (apply ck-oper (append (list '>>) args)))
(define (ck-bit-shift-l . args) (apply ck-oper (append (list '<<) args)))
(define (ck-bit-and . args) (apply ck-oper (append (list '&) args)))
(define (ck-bit-or . args) (apply ck-oper (append (list (string->symbol "|")) args)))
(define (ck-bit-xor . args) (apply ck-oper (append (list '^) args)))
(define (ck-inc arg) (format "~a++" arg))
(define (ck-dec arg) (format "~a--" arg))
(define (ck-not arg) (format "!~a" arg))
(define (ck-positive arg) (format "+~a" arg))
(define (ck-negate arg) (format "+~a" arg))
(define (ck-new arg) (format "new ~a" arg))

;; Control structures
(define (ck-if expr . stmts)
  (format "if (~a) {\n~a\n}" expr (apply ck-do stmts)))
(define (ck-else-if expr . stmts)
  (format "else if (~a) {\n~a\n}" expr (apply ck-do stmts)))
(define (ck-else . stmts)
  (format "else {\n~a\n}" (apply ck-do stmts)))
(define (ck-while expr . stmts)
  (format "while (~a) {\n~a\n}" expr (apply ck-do stmts)))
(define (ck-repeat expr . stmts)
  (format "repeat (~a) {\n~a\n}" expr (apply ck-do stmts)))
(define (ck-until expr . stmts)
  (format "until (~a) {\n~a\n}" expr (apply ck-do stmts)))
(define (ck-do-while expr . stmts)
  (format "do {\n~a\n} while (~a)" expr (apply ck-do stmts)))
(define (ck-do-until expr . stmts)
  (format "do {\n~a\n} until (~a)" expr (apply ck-do stmts)))
(define (ck-for expr1 expr2 expr3 . stmts)
  (format "for (~a; ~a; ~a) {\n~a\n}" expr1 expr2 expr3 (apply ck-do stmts)))
(define ck-break 'break)
(define ck-continue 'continue)

#| (displayln |#
#|   (ck-do |#
#|     ; Function definition |#
#|     (ck-fun ck-void 'greet (list (ck-decl 'int 'name)) |#
#|       (ck-+ (ck-string "Hello, ") |#
#|             'name |#
#|             (ck-string "!"))) |#
#|     ; Function call |#
#|     (ck-<= ck-chout |#
#|            (ck-call 'greet (ck-string "Bob")) |#
#|            ck-newline) |#
#|     ; Array definition |#
#|     (ck-@=> (ck-array (ck-int 1)) |#
#|             (ck-array-decl 'int 'name)) |#
#|     ; Array getting and setting |#
#|     (ck-<<<>>> (ck-array-ref 'name (ck-int 0))) |#
#|     (ck-=> (ck-int 1) |#
#|            (ck-array-ref 'name (ck-int 0))) |#
#|     ; Addition |#
#|     (ck-<<<>>> |#
#|       (ck-+ (ck-int 1) |#
#|             (ck-int 2) |#
#|             (ck-int 3))) |#
#|     ; Declaring variables |#
#|     (ck-=> (ck-int 1) |#
#|            (ck-decl 'int 'foo)) |#
#|     (ck-=> (ck-int 2) |#
#|            (ck-decl 'int 'bar)) |#
#|     (ck-=> (ck-int 3) |#
#|            (ck-decl 'int 'baz)) |#
#|     ; ck-oper |#
#|     (ck-oper '<= ck-chout |#
#|                  (ck-oper '+ 'foo 'bar 'baz) |#
#|                  ck-newline) |#
#|     ; Addition on variables and outputting |#
#|     (ck-<= ck-chout |#
#|            (ck-+ 'foo 'bar 'baz) |#
#|            ck-newline) |#
#|     ; Another declaration |#
#|     (ck-=> (ck-int 0) |#
#|            (ck-decl 'int 'n)) |#
#|     ; While loop |#
#|     (ck-while |#
#|       'true |#
#|       ; Incrementing a variable |#
#|       (ck-=> (ck-oper '+ 'n (ck-int 1)) |#
#|              'n) |#
#|       ; Outputting the variable |#
#|       (ck-<= ck-chout |#
#|              'n |#
#|              ck-newline)) |#
#|     ; Progressing time |#
#|     (ck-=> (ck-dur (ck-int 500) ck-ms) |#
#|            ck-now) |#

#|     (ck-class 'Foo 'public #f |#
#|       (ck-<<<>>> (ck-string "Inside class Foo!"))) |#

#|     (ck-class 'Bar 'public 'Chugin |#
#|       (ck-<<<>>> (ck-string "Inside class Bar!"))) |#
#|   ) |#
#| ) |#
