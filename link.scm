(c-declare #<<c-declare-end

#include "testlib.h"

c-declare-end
)

(##include "scmlib.scm")

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

