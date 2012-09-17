(declare
 (standard-bindings)
 (extended-bindings)
 (not safe)
 (flonum))

(define-structure vect x y)

(define (vect-make x y)
  (make-vect (exact->inexact x) (exact->inexact y)))

(define (vect-add-into! r a b)
  (let ((xa (vect-x a))
        (xb (vect-x b))
        (ya (vect-y a))
        (yb (vect-y b)))
   (vect-x-set! r (fl+ xa xb))
   (vect-y-set! r (fl+ ya yb))
   r))

(define (vect-sub-into! r a b)
  (let ((xa (vect-x a))
        (xb (vect-x b))
        (ya (vect-y a))
        (yb (vect-y b)))
   (vect-x-set! r (fl- xa xb))
   (vect-y-set! r (fl- ya yb))
   r))

(define (vect-scale-into! r a s)
  (let ((xa (vect-x a))
        (ya (vect-y a)))
   (vect-x-set! r (fl* xa (exact->inexact s)))
   (vect-y-set! r (fl* ya (exact->inexact s)))
   r))

(define (vect-scale-both-into! r a sx sy)
  (let ((xa (vect-x a))
        (ya (vect-y a)))
   (vect-x-set! r (fl* xa (exact->inexact sx)))
   (vect-y-set! r (fl* ya (exact->inexact sy)))
   r))

(define (vect-scale a s)
  (vect-scale-into! (vect-make 0 0) a s))

(define (vect-copy v)
  (make-vect (vect-x v) (vect-y v)))

(define (dist x1 y1 x2 y2)
  (let ((dx (- x1 x2))
        (dy (- y1 y2)))
    (sqrt (fl+ (fl* dx dx) (fl* dy dy)))))

(define-structure particle r dr t dt s ds)

(define (particle-make r dr t dt s ds)
  (make-particle r dr
                 (exact->inexact t) (exact->inexact dt)
                 (exact->inexact s) (exact->inexact ds)))

(define (particle-x p)
  (vect-x (particle-r p)))

(define (particle-y p)
  (vect-y (particle-r p)))

(define (particle-integrate p dt)
  (let ((dt (exact->inexact dt)))
    (vect-add-into! (particle-r p)
                    (particle-r p)
                    (vect-scale (particle-dr p) dt))
    (particle-t-set! p (fl+ (particle-t p)
                            (fl* (particle-dt p) dt)))
    (particle-s-set! p (flmax 0. (fl+ (particle-s p)
                                   (fl* (particle-ds p) dt))))))

(define (random-vector maxx maxy)
  (let ((maxx (exact->inexact maxx))
        (maxy (exact->inexact maxy)))
   (vect-make (rand-in-range (fl- maxx) maxx)
              (rand-in-range (fl- maxy) maxy))))

(define (rand-in-range min max)
  (let ((min (->fixnum min))
        (max (->fixnum max)))
   (fx+ min (random-integer (fx- max min)))))

(define pi 3.141592654)

(define (degrees->radians deg)
  (/ (* pi deg) 180.0))

(define (radians->degrees rad)
  (/ (* rad 180.0) pi))


