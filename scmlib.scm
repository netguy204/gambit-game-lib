#!/usr/bin/env gsc-script

(c-declare #<<c-declare-end

#include <stdio.h>
#include "testlib.h"

c-declare-end
)

(define-macro (when test . body)
  `(if ,test
       (begin . ,body)
       '()))

(define-macro (comment . body)
  ''())

(define-structure vector x y)

(define (vector-add-into! r a b)
  (let ((xa (vector-x a))
        (xb (vector-x b))
        (ya (vector-y a))
        (yb (vector-y b)))
   (vector-x-set! r (+ xa xb))
   (vector-y-set! r (+ ya yb))
   r))

(define (vector-sub-into! r a b)
  (let ((xa (vector-x a))
        (xb (vector-x b))
        (ya (vector-y a))
        (yb (vector-y b)))
   (vector-x-set! r (- xa xb))
   (vector-y-set! r (- ya yb))
   r))

(define (vector-scale-into! r a s)
  (let ((xa (vector-x a))
        (ya (vector-y a)))
   (vector-x-set! r (* xa s))
   (vector-y-set! r (* ya s))
   r))

(define (vector-scale-both-into! r a sx sy)
  (let ((xa (vector-x a))
        (ya (vector-y a)))
   (vector-x-set! r (* xa sx))
   (vector-y-set! r (* ya sy))
   r))

(define (vector-scale a s)
  (vector-scale-into! (make-vector 0 0) a s))

(define-structure particle r dr)

(define (particle-x p)
  (vector-x (particle-r p)))

(define (particle-y p)
  (vector-y (particle-r p)))

(define (particle-integrate p msecs)
  (vector-add-into! (particle-r p)
                    (particle-r p) (vector-scale (particle-dr p)
                                                 (/ msecs 1000))))

(define *p* (make-particle (make-vector 5 5) (make-vector 50 50)))

(define (update-view msecs)
  (let ((x (particle-x *p*))
        (y (particle-y *p*))
        (dr (particle-dr *p*)))
    (when (or (>= x 640) (<= x 0))
          (vector-scale-both-into! dr dr -1 1))
    (when (or (>= y 480) (<= y 0))
          (vector-scale-both-into! dr dr 1 -1))

    (particle-integrate *p* msecs)
    (blit-image *screen* *test-image* (round (particle-x *p*)) (round (particle-y *p*)))))

(define load-image 
  (c-lambda (nonnull-char-string)
            (pointer void)
            "load_image"))

(define blit-image
  (c-lambda ((pointer void) (pointer void) int int)
            void
            "blit_image"))

(c-define (set-screen scr) ((pointer void)) void "set_screen" ""
          (set! *screen* scr)
          (set! *test-image* (load-image "test.png")))

(c-define (step msecs) (int) void "step" ""
          (when *test-image*
                (update-view msecs)))

(c-define (terminate) () void "terminate" ""
          (display "terminating") (newline))
