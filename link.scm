(c-declare #<<c-declare-end

#include "testlib.h"

c-declare-end
)

(load "scmlib")

(##heartbeat-interval-set! 0.5)

;(##include "scmlib.scm")

(c-define-type ImageResource (pointer (struct "ImageResource_")))
(c-define-type Clock (pointer (struct "Clock_")))
(c-define-type Sprite (pointer (struct "Sprite_")))
(c-define-type SpriteList (pointer (struct "SpriteList_")))
(c-define-type InputState (pointer (struct "InputState_")))

(define image-load-internal
  (c-lambda (nonnull-char-string)
            ImageResource
            "image_load"))

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

(define (comp2 f g)
  (lambda (item x) (f item (g x))))

(define (compose f g)
  (lambda (x) (f (g x))))

(define %clock-update
  (c-lambda (Clock float)
            float
            "clock_update"))

(define clock-update (comp2 %clock-update exact->inexact))

(define clock-time
  (c-lambda (Clock)
            long
            "clock_time"))

(define %cycles->seconds
  (c-lambda (long)
            float
            "clock_cycles_to_seconds"))

(define %seconds->cycles
  (c-lambda (float)
            long
            "clock_seconds_to_cycles"))

(define input-leftright
  (c-lambda (InputState)
	    float
	    "___result = ___arg1->leftright;"))

(define input-updown
  (c-lambda (InputState)
	    float
	    "___result = ___arg1->updown;"))

(define %input-action1
  (c-lambda (InputState)
            int
            "___result = ___arg1->action1;"))

(define (input-action1 input)
  (= 1 (%input-action1 input)))

(define cycles->seconds (compose %cycles->seconds inexact->exact))
(define seconds->cycles (compose %seconds->cycles exact->inexact))

(define *screen-width* #f)

(define *screen-height* #f)

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

(define %sprite-width-set!
  (c-lambda (Sprite float)
            void
            "___arg1->w = ___arg2;"))

(define %sprite-height-set!
  (c-lambda (Sprite float)
            void
            "___arg1->h = ___arg2;"))



(define sprite-x-set! (comp2 %sprite-x-set! exact->inexact))
(define sprite-y-set! (comp2 %sprite-y-set! exact->inexact))
(define sprite-origin-x-set! (comp2 %sprite-origin-x-set! exact->inexact))
(define sprite-origin-y-set! (comp2 %sprite-origin-y-set! exact->inexact))
(define sprite-angle-set! (comp2 %sprite-angle-set! exact->inexact))
(define sprite-width-set! (comp2 %sprite-width-set! exact->inexact))
(define sprite-height-set! (comp2 %sprite-height-set! exact->inexact))

(define %sprite-parms-set!
  ;; sprite image x y cx cy angle width height
  (c-lambda (Sprite ImageResource float float float float float float float)
            void
            "
___arg1->resource = ___arg2;
___arg1->displayX = ___arg3;
___arg1->displayY = ___arg4;
___arg1->originX = ___arg5;
___arg1->originY = ___arg6;
___arg1->angle = ___arg7;
___arg1->w = ___arg8;
___arg1->h = ___arg9;
"))

(define (sprite-parms-set! sprite img x y cx cy angle width height)
  (%sprite-parms-set! sprite img
                      (exact->inexact x) (exact->inexact y)
                      (exact->inexact cx) (exact->inexact cy)
                      (exact->inexact angle)
                      (exact->inexact width) (exact->inexact height)))

(define %sprite-coords-set!
  ;; u0, v0, u1, v1
  (c-lambda (Sprite float float float float)
      void
"
___arg1->u0 = ___arg2;
___arg1->v0 = ___arg3;
___arg1->u1 = ___arg4;
___arg1->v1 = ___arg5;"))

(define (sprite-coords-set! sprite u0 v0 u1 v1)
  (%sprite-coords-set! sprite
                       (exact->inexact u0) (exact->inexact v0)
                       (exact->inexact u1) (exact->inexact v1)))

(define frame/spritelist-append
  (c-lambda (SpriteList Sprite)
            SpriteList
            "frame_spritelist_append"))

(define spritelist-enqueue-for-screen!
  (c-lambda (SpriteList)
            void
            "spritelist_enqueue_for_screen"))

;;; audio
(c-define-type Sampler (pointer (struct "Sampler_")))

(define %sinsampler-make
  (c-lambda (long long float float float)
            Sampler
            "sinsampler_make"))

(define (sinsampler-make start duration freq amp phase)
  (%sinsampler-make (inexact->exact start)
                    (inexact->exact duration)
                    (exact->inexact freq)
                    (exact->inexact amp)
                    (exact->inexact phase)))

(define %sawsampler-make
  (c-lambda (long long float float float)
            Sampler
            "sawsampler_make"))

(define (sawsampler-make start duration freq amp phase)
  (%sawsampler-make (inexact->exact start)
                    (inexact->exact duration)
                    (exact->inexact freq)
                    (exact->inexact amp)
                    (exact->inexact phase)))

(define *sample-freq* ((c-lambda () long "___result = SAMPLE_FREQ;")))
(define *num-channels* 2)

(define (seconds->samples seconds)
  (* *num-channels* *sample-freq* seconds))

(define audio-current-sample
  (c-lambda ()
            long
            "audio_current_sample"))

(define audio-enqueue
  (c-lambda (Sampler)
            void
            "audio_enqueue"))

;;; game lifecycle
(define *game-clock* #f)

;; wild hack to keep gambit from trying to kill our process before
;; we're done. We use a continuation to send the gambit exit system
;; off into space after we've told the C side that we need to be torn
;; down when it's ready. Seems to only work on non-arm. Call exit
;; instead of giving the repl a ,q

(define ##exit-cc-hack #f)
(call/cc (lambda (cc) (set! ##exit-cc-hack cc)))

(define (exit)
  (terminate)
  (%notify-terminate))

(define quit exit)

(c-define (scm-init) () void "scm_init" ""
          (set! *game-clock* (clock-make))
          (##add-exit-job!
           (lambda ()
	     (exit)
	     (##exit-cc-hack)))

          (set! *screen-width*
                ((c-lambda () int "___result = screen_width;")))
          (set! *screen-height*
                ((c-lambda () int "___result = screen_height;")))

          ;(clock-time-scale-set! *game-clock* 0.2)
          (display "initializing") (newline)
          (ensure-resources)
          (thread-start!
           (make-thread
            (lambda ()
              (##repl-debug-main)))))

;;; resource lifecycle
(define *resources* (make-table))

(define (image-load path)
  (let ((resource (table-ref *resources* path #f)))
    (if resource resource
        (begin
          (let ((new-resource (image-load-internal path)))
            (table-set! *resources* path new-resource)
            new-resource)))))

(c-define (resources-released) () void "resources_released" ""
          (set! *resources* (make-table)))

;;; gameloop
(c-define (step msecs input) (int InputState) void "step" ""
          (update-view (clock-update *game-clock* (/ msecs 1000.0)) input))


;;; termination

;; c tells us to terminate
(c-define (terminate) () void "terminate" ""
          (display "terminating") (newline))

;; we tell c to terminate
(define %notify-terminate
  (c-lambda ()
            void
            "notify_gambit_terminated"))
