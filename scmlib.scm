#!/usr/bin/env gsc-script

(c-declare #<<c-declare-end

#include <stdio.h>
#include "testlib.h"

c-declare-end
)

(define fopen
  (c-lambda (nonnull-char-string nonnull-char-string)
            (pointer "FILE")
            "fopen"))

(define fclose
  (c-lambda ((pointer "FILE"))
            int
            "fclose"))

(define fgetc
  (c-lambda ((pointer "FILE"))
            int
            "fgetc"))

(define feof
  (c-lambda ((pointer "FILE"))
            int
            "feof"))

(define (eof? file)
  (not (= (feof file) 0)))

(define stdin
  ((c-lambda () (pointer "FILE") "___result_voidstar = stdin;")))

(define printf
  (c-lambda (nonnull-char-string  nonnull-char-string)
            int
            "printf"))

(define (slurp fname)
  (define (iter f res)
    (if (eof? f)
        res
        (iter f (cons (fgetc f) res))))

  (let* ((f (fopen fname "r"))
         (r (iter f '())))
    (fclose f)
    r))

(define fib
  (c-lambda (int)
            int
            "fib"))


(c-define (enum-object x) (int) void "enum_object" ""
          (write x)
          (newline))
