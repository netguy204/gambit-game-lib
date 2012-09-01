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

