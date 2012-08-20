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
                (blit-image *screen* *test-image* 50 50))
          (display "stepping") (newline))

(c-define (terminate) () void "terminate" ""
          (display "terminating") (newline))
