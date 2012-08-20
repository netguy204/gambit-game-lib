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


(define-structure particle r dr)

(define (particle-x p)
  (vector-x (particle-r p)))

(define (particle-y p)
  (vector-y (particle-r p)))

(define (particle-integrate p)
  (vector-add-into! (particle-r p)
                    (particle-r p) (particle-dr p)))

(define *p* (make-particle (make-vector 0 0) (make-vector 1 1)))

(define (update-view)
  (particle-integrate *p*)
  (blit-image *screen* *test-image* (particle-x *p*) (particle-y *p*)))

(define load-image 
  (c-lambda (nonnull-char-string)
            (pointer "SDL_Surface")
            "load_image"))

(define blit-image
  (c-lambda ((pointer "SDL_Surface") (pointer "SDL_Surface") int int)
            void
            "blit_image"))

(c-define (set-screen scr) ((pointer "SDL_Surface")) void "set_screen" ""
          (set! *screen* scr)
          (set! *test-image* (load-image "test.png")))

(c-define (step) () void "step" ""
          (when *test-image*
                (update-view)))

(c-define (terminate) () void "terminate" ""
          (display "terminating") (newline))
