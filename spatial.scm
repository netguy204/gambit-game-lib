(define-structure spatial obj->idxs idx->objs width)

(define (spatial-calc-idx width x y)
  (+ x (* y width)))

(define (spatial-idx spatial x y)
  (spatial-calc-idx (spatial-width spatial) x y))

(define (spatial-rect->idxs spatial rect)
  (let ((width (spatial-width spatial))
        (minx (floor (rect-minx rect)))
        (maxx (ceiling (rect-maxx rect)))
        (miny (floor (rect-miny rect)))
        (maxy (ceiling (rect-maxy rect)))
        (result '()))
    (let loopy ((y miny))
      (if (< y maxy)
          (let loopx ((x minx))
            (if (< x maxx)
                (begin
                  (set! result (cons (spatial-calc-idx width x y) result))
                  (loopx (+ x 1)))
                (loopy (+ y 1))))
          result))))

(define (spatial-append-unique-pred pred new old)
  (let loop ((new new)
             (old old))
    (if (null? new)
        old
        (if (some? (lambda (x) (pred x (car new))) old)
            (loop (cdr new) old)
            (loop (cdr new) (cons (car new) old))))))

(define (spatial-rect spatial rect)
  (let ((idxs (spatial-rect->idxs spatial rect))
        (idx->objs (spatial-idx->objs spatial)))

    (let loop ((idxs idxs)
               (result '()))
      (if (null? idxs)
          result
          (loop (cdr idxs)
                (spatial-append-unique-pred eq?
                  (table-ref idx->objs (car idxs) '())
                  result))))))

(define (spatial-delete! spatial value)
  (let* ((obj->idxs (spatial-obj->idxs spatial))
         (idx->objs (spatial-idx->objs spatial))
         (idxs (table-ref obj->idxs value '())))
    (let loop ((idxs idxs))
      (if (not (null? idxs))
          ;; remove this object from the index table
          (let* ((objs (table-ref idx->objs (car idxs)))
                 (objs (remove-if (lambda (o) (eq? o value)) objs)))
            (if (null? objs)
                (table-set! idx->objs (car idxs))
                (table-set! idx->objs (car idxs) objs))
            (loop (cdr idxs)))))
    ;; remove this object from the object table
    (table-set! obj->idxs value)))

(define (spatial-update! spatial value rect)
  (spatial-delete! spatial value)
  (let ((idxs (spatial-rect->idxs spatial rect))
        (obj->idxs (spatial-obj->idxs spatial))
        (idx->objs (spatial-idx->objs spatial)))
    (table-set! obj->idxs value idxs)
    (let loop ((idxs idxs))
      (if (not (null? idxs))
          (let ((values (table-ref idx->objs (car idxs) '())))
            (table-set! idx->objs (car idxs) (cons value values))
            (loop (cdr idxs))))))
  value)

(define (spatial-objects spatial)
  (map car (table->list (spatial-obj->idxs spatial))))

(define (spatial-make width)
  (make-spatial (make-table test: eq?) (make-table) width))
