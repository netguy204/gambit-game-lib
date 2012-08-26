(##include "math.scm")
(##include "common.scm")

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

(define *ps* #f)

(define (call-with-resources fn)
  (parameterize ((*test-image* (image-load "test.png")))
    (fn)))

(define (ensure-resources)
  (call-with-resources
   (lambda ()
     (set! *ps* (repeatedly random-particle 100)))))

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
    (sprite-x-set! sprite (exact->inexact (particle-x p)))
    (sprite-y-set! sprite (exact->inexact (particle-y p)))
    (frame/spritelist-append sprite-list sprite)))

(define (update-view dt)
  (call-with-resources
   (lambda ()
     (let ((w (- 640 (image-width (*test-image*))))
           (h (- 480 (image-height (*test-image*)))))
       (let loop ((ps *ps*)
                  (sprite-list #f))
         (if (null? ps)
             (if sprite-list (spritelist-render-to-screen! sprite-list))
             (loop (cdr ps) (update-particle sprite-list (car ps) dt w h))))))))

