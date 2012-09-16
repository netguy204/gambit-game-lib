(declare
 (standard-bindings)
 (extended-bindings)
 (not safe)
 (flonum))

(define-structure rect minx miny maxx maxy)

(define (rect-make minx miny maxx maxy)
  (make-rect (exact->inexact minx) (exact->inexact miny)
             (exact->inexact maxx) (exact->inexact maxy)))

(define (rect-width rect)
  (fl- (rect-maxx rect) (rect-minx rect)))

(define (rect-height rect)
  (fl- (rect-maxy rect) (rect-miny rect)))

(define (rect-make-point x y)
  (let* ((minx (floor x))
         (maxx (ceiling x))
         (miny (floor y))
         (maxy (ceiling y))

         (maxx (if (fl= minx maxx)
                   (fl+ maxx 1)
                   maxx))
         (maxy (if (fl= miny maxy)
                   (fl+ maxy 1)
                   maxy)))
    (rect-make minx miny maxx maxy)))

(define (rect-make-radius cx cy radius)
  (rect-make (max 0. (fl- cx radius))
             (max 0. (fl- cy radius))
             (fl+ cx radius)
             (fl+ cy radius)))

(define (rect-intersect a b)
  (let ((aminx (rect-minx a))
        (aminy (rect-miny a))
        (amaxx (rect-maxx a))
        (amaxy (rect-maxy a))

        (bminx (rect-minx b))
        (bminy (rect-miny b))
        (bmaxx (rect-maxx b))
        (bmaxy (rect-maxy b)))

    (cond
     ((fl< amaxx bminx) #f)
     ((fl> aminx bmaxx) #f)
     ((fl< amaxy bminy) #f)
     ((fl> aminy bmaxy) #f)
     (#t #t))))
