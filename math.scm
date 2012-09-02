(declare
 (standard-bindings)
 (not safe))

(define-structure vect x y)

(define (vect-add-into! r a b)
  (let ((xa (vect-x a))
        (xb (vect-x b))
        (ya (vect-y a))
        (yb (vect-y b)))
   (vect-x-set! r (+ xa xb))
   (vect-y-set! r (+ ya yb))
   r))

(define (vect-sub-into! r a b)
  (let ((xa (vect-x a))
        (xb (vect-x b))
        (ya (vect-y a))
        (yb (vect-y b)))
   (vect-x-set! r (- xa xb))
   (vect-y-set! r (- ya yb))
   r))

(define (vect-scale-into! r a s)
  (let ((xa (vect-x a))
        (ya (vect-y a)))
   (vect-x-set! r (* xa s))
   (vect-y-set! r (* ya s))
   r))

(define (vect-scale-both-into! r a sx sy)
  (let ((xa (vect-x a))
        (ya (vect-y a)))
   (vect-x-set! r (* xa sx))
   (vect-y-set! r (* ya sy))
   r))

(define (vect-scale a s)
  (vect-scale-into! (make-vect 0 0) a s))

(define (vect-copy v)
  (make-vect (vect-x v) (vect-y v)))

(define (dist x1 y1 x2 y2)
  (let ((dx (- x1 x2))
        (dy (- y1 y2)))
    (sqrt (+ (* dx dx) (* dy dy)))))
