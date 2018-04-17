#lang racket

(require "utils.rkt")

(provide (all-defined-out))

;; Types and values
(define (int n) (format "~a" n))
(define (float n) (format "~a" n))
(define (string s) (format "~s" s))
(define (dur n t) (format "~a::~a" n t))
(define (complex r i) (format "#(~a, ~a)" r i))
(define (polar m p) (format "%(~a, ~a)" m p))
(define (array . args) (format "[~a]" (apply exprs args)))
(define (array-ref name key) (format "~a[~a]" name key))
(define now 'now)
(define pi 'pi)
(define void 'void)
(define true 'true)
(define false 'false)
(define null 'null)
(define me 'me)
(define samp 'samp)
(define ms 'ms)
(define second 'second)
(define minute 'minute)
(define hour 'hour)
(define day 'day)
(define week 'week)
(define chout 'chout)
(define cherr 'cherr)
(define newline (string->symbol "IO.newline()"))
(define adc 'adc)
(define dac 'dac)
(define blackhole 'blackhole)

;; Classes
(define (class name attr extends . body)
  (define baked-attr (if attr (format "~a " attr) ""))
  (define baked-extends (if extends (format " extends ~a" extends) ""))
  (define baked-body (apply do body))
  (format "~a~a ~a~a {\n~a\n}" baked-attr "class" name baked-extends baked-body))
(define (ref name key) (format "~a.~a" name key))
(define (ref-call name fn . args) (format "~a.~a" name (apply call (append (list fn) args))))
(define this 'this)

;; Basics
(define (do . stmts)
  (string-join (map (λ (stmt) (format "~a;" stmt)) stmts) "\n"))
(define (exprs . stmts)
  (string-join (map (λ (stmt) (format "~a" stmt)) stmts) ", "))
(define (<<<>>> . args)
  (format "<<< ~a >>>" (apply exprs args)))
(define (oper op . args)
  (string-join (map (λ (n) (format "~a" n)) args) (format " ~a " op)))
(define (decl t name) (format "~a ~a" t name))
(define (static-decl t name) (format "static ~a ~a" t name))
(define (array-decl t name) (format "~a ~a[]" t name))
(define (call fn . args)
  (format "~a(~a)" fn (apply exprs args)))
(define (fun t name params . body)
  (format "fun ~a ~a (~a) {\n~a\n}" t name (apply exprs params) (apply do body)))
(define (static-fun t name params . body)
  (format "fun static ~a ~a (~a) {\n~a\n}" t name (apply exprs params) (apply do body)))
(define (return v) (format "return ~a" v))
(define (spork v [ref #f])
  (if ref
    (format "spork ~~ ~a @ ~a" v ref)
    (format "spork ~~ ~a" v)))

;; Operators
(define (=> . args) (apply oper (append (list '=>) args)))
(define (=< . args) (apply oper (append (list '=>) args)))
(define (=^ . args) (apply oper (append (list '@=>) args)))
(define (@=> . args) (apply oper (append (list '@=>) args)))
(define (+ . args) (apply oper (append (list '+) args)))
(define (- . args) (apply oper (append (list '-) args)))
(define (* . args) (apply oper (append (list '*) args)))
(define (/ . args) (apply oper (append (list '/) args)))
(define (% . args) (apply oper (append (list '%) args)))
(define (+=> . args) (apply oper (append (list '+=>) args)))
(define (-=> . args) (apply oper (append (list '-=>) args)))
(define (*=> . args) (apply oper (append (list '*=>) args)))
(define (/=> . args) (apply oper (append (list '/=>) args)))
(define (%=> . args) (apply oper (append (list '%=>) args)))
(define (cast . args) (apply oper (append (list '$) args)))
(define (and . args) (apply oper (append (list '&&) args)))
(define (or . args) (apply oper (append (list '||) args)))
(define (equal? . args) (apply oper (append (list '==) args)))
(define (not-equal? . args) (apply oper (append (list '!=) args)))
(define (> . args) (apply oper (append (list '>) args)))
(define (>= . args) (apply oper (append (list '>=) args)))
(define (< . args) (apply oper (append (list '<) args)))
(define (<= . args) (apply oper (append (list '<=) args)))
(define (bit-shift-r . args) (apply oper (append (list '>>) args)))
(define (bit-shift-l . args) (apply oper (append (list '<<) args)))
(define (bit-and . args) (apply oper (append (list '&) args)))
(define (bit-or . args) (apply oper (append (list (string->symbol "|")) args)))
(define (bit-xor . args) (apply oper (append (list '^) args)))
(define (inc arg) (format "~a++" arg))
(define (dec arg) (format "~a--" arg))
(define (not arg) (format "!~a" arg))
(define (positive arg) (format "+~a" arg))
(define (negate arg) (format "+~a" arg))
(define (new arg) (format "new ~a" arg))

;; Control structures
(define (if expr . stmts)
  (format "if (~a) {\n~a\n}" expr (apply do stmts)))
(define (else-if expr . stmts)
  (format "else if (~a) {\n~a\n}" expr (apply do stmts)))
(define (else . stmts)
  (format "else {\n~a\n}" (apply do stmts)))
(define (while expr . stmts)
  (format "while (~a) {\n~a\n}" expr (apply do stmts)))
(define (repeat expr . stmts)
  (format "repeat (~a) {\n~a\n}" expr (apply do stmts)))
(define (until expr . stmts)
  (format "until (~a) {\n~a\n}" expr (apply do stmts)))
(define (do-while expr . stmts)
  (format "do {\n~a\n} while (~a)" expr (apply do stmts)))
(define (do-until expr . stmts)
  (format "do {\n~a\n} until (~a)" expr (apply do stmts)))
(define (for expr1 expr2 expr3 . stmts)
  (format "for (~a; ~a; ~a) {\n~a\n}" expr1 expr2 expr3 (apply do stmts)))
(define break 'break)
(define continue 'continue)

#| (displayln |#
#|   (do |#
#|     ; Function definition |#
#|     (fun void 'greet (list (decl 'int 'name)) |#
#|       (+ (string "Hello, ") |#
#|          'name |#
#|          (string "!"))) |#
#|     ; Function call |#
#|     (<= chout |#
#|         (call 'greet (string "Bob")) |#
#|         newline) |#
#|     ; Array definition |#
#|     (@=> (array (int 1)) |#
#|          (array-decl 'int 'name)) |#
#|     ; Array getting and setting |#
#|     (<<<>>> (array-ref 'name (int 0))) |#
#|     (=> (int 1) |#
#|         (array-ref 'name (int 0))) |#
#|     ; Addition |#
#|     (<<<>>> |#
#|       (+ (int 1) |#
#|          (int 2) |#
#|          (int 3))) |#
#|     ; Declaring variables |#
#|     (=> (int 1) |#
#|         (decl 'int 'foo)) |#
#|     (=> (int 2) |#
#|         (decl 'int 'bar)) |#
#|     (=> (int 3) |#
#|         (decl 'int 'baz)) |#
#|     ; oper |#
#|     (oper '<= chout |#
#|               (oper '+ 'foo 'bar 'baz) |#
#|               newline) |#
#|     ; Addition on variables and outputting |#
#|     (<= chout |#
#|         (+ 'foo 'bar 'baz) |#
#|         newline) |#
#|     ; Another declaration |#
#|     (=> (int 0) |#
#|         (decl 'int 'n)) |#
#|     ; While loop |#
#|     (while |#
#|       'true |#
#|       ; Incrementing a variable |#
#|       (=> (oper '+ 'n (int 1)) |#
#|           'n) |#
#|       ; Outputting the variable |#
#|       (<= chout |#
#|           'n |#
#|           newline)) |#
#|     ; Progressing time |#
#|     (=> (dur (int 500) ms) |#
#|         now) |#

#|     (class 'Foo 'public #f |#
#|       (<<<>>> (string "Inside class Foo!"))) |#

#|     (class 'Bar 'public 'Chugin |#
#|       (<<<>>> (string "Inside class Bar!"))) |#
#|   ) |#
#| ) |#
