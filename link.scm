(c-declare #<<c-declare-end

#include "testlib.h"

c-declare-end
)

(load "scmlib")
;(##include "scmlib.scm")

(c-define-type ImageResource (pointer (struct "ImageResource_")))
(c-define-type Clock (pointer (struct "Clock_")))
(c-define-type Sprite (pointer (struct "Sprite_")))
(c-define-type SpriteList (pointer (struct "SpriteList_")))

(define image-load-internal
  (c-lambda (nonnull-char-string)
            ImageResource
            "image_load"))

(define image-render-to-screen!
  (c-lambda (ImageResource float float float float float)
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

(define images-free
  (c-lambda ()
            void
            "images_free"))

(define clock-free
  (c-lambda (Clock)
            void
            "clock_free"))

(define clock-make
  (c-lambda ()
            Clock
            "clock_make"))

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

(define cycles->seconds
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

(define %sprite-x-set!
  (c-lambda (Sprite float)
            void
            "___arg1->displayX = ___arg2;"))

(define %sprite-y-set!
  (c-lambda (Sprite float)
            void
            "___arg1->displayY = ___arg2;"))

(define %sprite-origin-x-set!
  (c-lambda (Sprite float)
            void
            "___arg1->originX = ___arg2;"))

(define %sprite-origin-y-set!
  (c-lambda (Sprite float)
            void
            "___arg1->originY = ___arg2;"))

(define %sprite-angle-set!
  (c-lambda (Sprite float)
            void
            "___arg1->angle = ___arg2;"))

(define (sprite-x-set! sprite val)
  (%sprite-x-set! sprite (exact->inexact val)))

(define (sprite-y-set! sprite val)
  (%sprite-y-set! sprite (exact->inexact val)))

(define (sprite-origin-x-set! sprite val)
  (%sprite-origin-x-set! sprite (exact->inexact val)))

(define (sprite-origin-y-set! sprite val)
  (%sprite-origin-y-set! sprite (exact->inexact val)))

(define (sprite-angle-set! sprite val)
  (%sprite-angle-set! sprite (exact->inexact val)))

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
(define *resources* (make-table))

(define (image-load path)
  (let ((resource (table-ref *resources* path #f)))
    (if resource resource
        (begin
          (let ((new-resource (image-load-internal path)))
            (table-set! *resources* path new-resource)
            new-resource)))))

(c-define (scm-init) () void "scm_init" ""
          (set! *game-clock* (clock-make))
          ;(clock-time-scale-set! *game-clock* 0.2)
          (display "initializing") (newline)
          (ensure-resources))

(c-define (step msecs) (int) void "step" ""
          (update-view (clock-update *game-clock* (/ msecs 1000.0))))

(c-define (terminate) () void "terminate" ""
          (display "terminating") (newline))

(c-define (resources-released) () void "resources_released" ""
          (set! *resources* (make-table)))



