#lang racket

(require compatibility/defmacro
         "ck-dsl.rkt")

#| (define a 1) |#
#| (ck |#
#|   (<<<>>> (string "Hello, world!") (array (int a) (int 2) (int 3))) |#
#|   (=> (dur 500 'ms) now)) |#

(ck
  (=> (int 1) (decl 'int 'running))
  (decl 'OscIn 'oin)
  (decl 'OscMsg 'msg)
  (=> (ref-call 'Std 'atoi (ref-call 'me 'arg (int 0)))
      (ref 'oin 'port))
  (ref-call 'oin 'addAddress (string "/quit,N"))
  (ref-call 'oin 'addAddress (string "/machine/status,N"))
  (ref-call 'oin 'addAddress (string "/machine/shreds,N"))
  (ref-call 'oin 'addAddress (string "/machine/add,s"))
  (ref-call 'oin 'addAddress (string "/machine/remove,i"))
  (ref-call 'oin 'addAddress (string "/machine/replace,is"))
  (<= chout (string "[engine] running") newline)
  (while (not-equal? 'running (int 0))
    (=> 'oin now)
    (while (not-equal? (ref-call 'oin 'recv 'msg) (int 0))
      (if (equal? (ref 'msg 'address) (string "/quit"))
        (=> (int 0) 'running))
      (if (equal? (ref 'msg 'address) (string "/machine/status"))
        (=> (ref-call 'Machine 'status) (decl 'int 'n))
        (<= chout (string "[engine] ") 'n newline))
      (if (equal? (ref 'msg 'address) (string "/machine/shreds"))
        (@=> (ref-call 'Machine 'shreds) (array-decl 'int 'shreds))
        (<= chout (string "[engine] ["))
        (for (=> (int 0) (decl 'int 'i))
             (< 'i (ref-call 'shreds 'size))
             (inc 'i)
          (<= chout (array-ref 'shreds 'i))
          (if (< 'i (- (ref-call 'shreds 'size) (int 1)))
            (<= chout (string ", "))))
        (<= chout (string "]") newline))
      (if (equal? (ref 'msg 'address) (string "/machine/add"))
        (<= chout
            (string "[engine] ")
            (ref-call 'Machine 'add (ref-call 'msg 'getString (int 0)))
            newline))
      (if (equal? (ref 'msg 'address) (string "/machine/remove"))
        (<= chout
            (string "[engine] ")
            (ref-call 'Machine 'remove (ref-call 'msg 'getInt (int 0)))
            newline))
      (if (equal? (ref 'msg 'address) (string "/machine/replace"))
        (<= chout
            (string "[engine] ")
            (ref-call 'Machine 'replace (ref-call 'msg 'getInt (int 0))
                                        (ref-call 'msg 'getString (int 1)))
            newline))))
  (<= chout (string "[engine] quit") newline))
