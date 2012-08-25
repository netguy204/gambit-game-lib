(c-declare #<<c-declare-end

#include "testlib.h"

c-declare-end
)

(load "scmlib")

(c-define-type ImageResource (pointer (struct "ImageResource_")))
(c-define-type Clock (pointer (struct "Clock_")))
(c-define-type Sprite (pointer (struct "Sprite_")))
(c-define-type SpriteList (pointer (struct "SpriteList_")))

(define image-load
  (c-lambda (nonnull-char-string)
            ImageResource
            "image_load"))

(define image-render-to-screen!
  (c-lambda (ImageResource float float)
            void
            "image_render_to_screen"))

(define image-width
  (c-lambda (ImageResource)
            int
            "image_width"))

(define image-height
  (c-lambda (ImageResource)
            int
            "image_height"))

(define clock-free
  (c-lambda (Clock)
            void
            "clock_free"))

(define (clock-make)
  (let ((clock ((c-lambda ()
                          Clock
                          "clock_make"))))
    (make-will clock
               (lambda (clock)
                 (clock-free clock)))
    clock))

(define clock-time-scale
  (c-lambda (Clock)
            float
            "___result = ___arg1->time_scale;"))

(define clock-time-scale-set!
  (c-lambda (Clock float)
            void
            "___arg1->time_scale = ___arg2;"))


(define clock-update
  (c-lambda (Clock float)
            float
            "clock_update"))

(define clock-time
  (c-lambda (Clock)
            long
            "clock_time"))

(define cycles->seonds
  (c-lambda (long)
            float
            "clock_cycles_to_seconds"))

(define seconds->cycles
  (c-lambda (float)
            long
            "clock_seconds_to_cycles"))

(define frame/make-sprite
  (c-lambda ()
            Sprite
            "frame_make_sprite"))

(define sprite-resource-set!
  (c-lambda (Sprite ImageResource)
            void
            "___arg1->resource = ___arg2;"))

(define sprite-x-set!
  (c-lambda (Sprite float)
            void
            "___arg1->displayX = ___arg2;"))

(define sprite-y-set!
  (c-lambda (Sprite float)
            void
            "___arg1->displayY = ___arg2;"))

(define frame/spritelist-append
  (c-lambda (SpriteList Sprite)
            SpriteList
            "frame_spritelist_append"))

(define spritelist-render-to-screen!
  (c-lambda (SpriteList)
            void
            "spritelist_render_to_screen"))

;;
(define *game-clock* #f)

(c-define (scm-init) () void "scm_init" ""
          (set! *game-clock* (clock-make))
          ;;(clock-time-scale-set! *game-clock* 2.0)
          (display "initializing") (newline)
          (ensure-resources))

(c-define (step msecs) (int) void "step" ""
          (update-view (clock-update *game-clock* (/ msecs 1000.0))))

(c-define (terminate) () void "terminate" ""
          (display "terminating") (newline))

