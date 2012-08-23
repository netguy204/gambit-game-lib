(##include "math.scm")
(##include "common.scm")


(define-structure particle r dr)

(define (particle-x p)
  (vector-x (particle-r p)))

(define (particle-y p)
  (vector-y (particle-r p)))

(define (particle-integrate p msecs)
  (vector-add-into! (particle-r p)
                    (particle-r p) (vector-scale (particle-dr p)
                                                 (/ msecs 1000))))

(define *p* (make-particle (make-vector 5 5) (make-vector 50 50)))

(define (update-view msecs)
  (let ((x (particle-x *p*))
        (y (particle-y *p*))
        (dr (particle-dr *p*)))
    (when (or (>= x 640) (<= x 0))
          (vector-scale-both-into! dr dr -1 1))
    (when (or (>= y 480) (<= y 0))
          (vector-scale-both-into! dr dr 1 -1))

    (particle-integrate *p* msecs)
    (blit-image *screen* *test-image* (round (particle-x *p*)) (round (particle-y *p*)))))

