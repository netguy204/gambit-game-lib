(declare
 (standard-bindings)
 (block))

(define-structure particle r dr)

(define (particle-x p)
  (vect-x (particle-r p)))

(define (particle-y p)
  (vect-y (particle-r p)))

(define (particle-integrate p dt)
  (vect-add-into! (particle-r p)
                  (particle-r p)
                  (vect-scale (particle-dr p) dt)))

(define (repeatedly fn n)
  (let loop ((result '())
             (n n))
    (if (> n 0)
        (loop (cons (fn) result)
              (- n 1))
        (reverse result))))

(define (rand-in-range min max)
  (+ min (random-integer (- max min))))

(define (random-particle)
  (make-particle (make-vect (random-integer 640) (random-integer 480))
                 (make-vect (rand-in-range -100 100) (rand-in-range -100 100))))

(define *ps* (repeatedly random-particle 1000))
(define *test-image* #f)

(define (ensure-resources)
  (if (not *test-image*)
    (set! *test-image* (load-image "test.png"))))

(define (update-view dt)

  (let loop ((ps *ps*)
             (sprite-list #f))
    (if (null? ps)
        (if sprite-list
            (render-spritelist-to-screen! sprite-list))
        
        (let* ((p (car ps))
               (x (particle-x p))
               (y (particle-y p))
               (dr (particle-dr p))
               (sprite (frame/make-sprite)))
      
          (if (or (>= x 640) (<= x 0))
                (vect-scale-both-into! dr dr -1 1))
          (if (or (>= y 480) (<= y 0))
                (vect-scale-both-into! dr dr 1 -1))
      
          (particle-integrate p dt)
          (sprite-resource-set! sprite *test-image*)
          (sprite-x-set! sprite (exact->inexact (particle-x p)))
          (sprite-y-set! sprite (exact->inexact (particle-y p)))

          (loop (cdr ps) (frame/spritelist-append sprite-list sprite))))))

