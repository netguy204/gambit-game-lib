(define-structure rect minx miny maxx maxy)

(define rect-make make-rect)

(define (rect-width rect)
  (- (rect-maxx rect) (rect-minx rect)))

(define (rect-height rect)
  (- (rect-maxy rect) (rect-miny rect)))
