(##include "math.scm")
(##include "common.scm")
(##include "spriter.scm")

(define *test-image* (make-parameter #f))

(define-structure particle r dr)

(define (particle-x p)
  (vect-x (particle-r p)))

(define (particle-y p)
  (vect-y (particle-r p)))

(define (particle-integrate p dt)
  (vect-add-into! (particle-r p)
                  (particle-r p)
                  (vect-scale (particle-dr p) dt)))

(define (rand-in-range min max)
  (+ min (random-integer (- max min))))

(define (random-particle)
  (let ((img-w (image-width (*test-image*)))
        (img-h (image-height (*test-image*))))
    (make-particle (make-vect (random-integer (- 640 img-w))
                              (random-integer (- 480 img-h)))
                   (make-vect (rand-in-range -100 100)
                              (rand-in-range -100 100)))))

(define (call-with-resources fn)
  (parameterize ((*test-image* (image-load "test.png")))
    (fn)))

(define *ps* #f)
(define *anim* #f)

(define (ensure-resources)
  (call-with-resources
   (lambda ()
     (set! *ps* (repeatedly random-particle 100))
     (set! *anim* (animation (entity (scml-load "monster/Example.SCML")
                                     "0")
                             "Idle")))))

(define (update-particle sprite-list p dt w h)
  (let* ((x (particle-x p))
         (y (particle-y p))
         (dr (particle-dr p))
         (sprite (frame/make-sprite)))
    
    (if (or (>= x w) (<= x 0))
        (vect-scale-both-into! dr dr -1 1))
    (if (or (>= y h) (<= y 0))
        (vect-scale-both-into! dr dr 1 -1))
    
    (particle-integrate p dt)
    (sprite-resource-set! sprite (*test-image*))
    (sprite-x-set! sprite (particle-x p))
    (sprite-y-set! sprite (particle-y p))
    (frame/spritelist-append sprite-list sprite)))

(define (update-view-old dt)
  (call-with-resources
   (lambda ()
     (let ((w (- 640 (image-width (*test-image*))))
           (h (- 480 (image-height (*test-image*)))))
       (let loop ((ps *ps*)
                  (sprite-list #f))
         (if (null? ps)
             (if sprite-list (spritelist-enqueue-for-screen! sprite-list))
             (loop (cdr ps)
                   (update-particle sprite-list
                                    (car ps) dt w h))))))))

(define (tkey->sprite tkey ox oy)
  (let* ((image (image-load (tkey-name tkey)))
         (sprite (frame/make-sprite))
         (h (image-height image))
         (w (image-width image))
         (px-img-offset (* (tkey-cx tkey) w))
         (py-img-offset (* (tkey-cy tkey) h))
         (piv-x (+ ox (tkey-x tkey)))
         (piv-y (+ oy (tkey-y tkey))))

    (sprite-resource-set! sprite image)
    (sprite-x-set! sprite piv-x)
    (sprite-y-set! sprite piv-y)
    (sprite-origin-x-set! sprite px-img-offset)
    (sprite-origin-y-set! sprite py-img-offset)

    (sprite-angle-set! sprite (tkey-angle tkey))
    sprite))

(define (add-animation sprite-list anim time ox oy)
  (reduce (lambda (sprite-list tkey)
            (frame/spritelist-append sprite-list
                                     (tkey->sprite tkey ox oy)))
          #f
          (reverse (interp-anim anim time))))

(define (update-view dt)
  (let* ((cycles-for-anim (seconds->cycles (/ (animation-length *anim*)
                                              1000.0)))
         (anim-cycle (modulo (clock-time *game-clock*) cycles-for-anim))
         (anim-time (cycles->seconds anim-cycle))
         (sprite-list (add-animation #f *anim* anim-time 320 100)))

    (update-view-old dt)
    (spritelist-enqueue-for-screen! sprite-list)))


(define (update-view-test dt)
  (spritelist-enqueue-for-screen!
   (frame/spritelist-append
    #f
    (let* ((sprite (frame/make-sprite))
           (cycles-per-rot (seconds->cycles 2.0))
           (rot-cycle (modulo (clock-time *game-clock*) cycles-per-rot))
           (angle (* 360.0 (/ rot-cycle cycles-per-rot)))
           (img (image-load "test.png"))
           (w (image-width img))
           (h (image-height img)))

      (sprite-resource-set! sprite img)
      (sprite-x-set! sprite 320)
      (sprite-y-set! sprite 240)
      (sprite-origin-x-set! sprite w)
      (sprite-origin-y-set! sprite h)
      (sprite-angle-set! sprite angle)
      sprite))))
