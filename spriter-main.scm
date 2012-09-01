(define (ensure-resources)
  (lambda ()
    (set! *ps* (repeatedly random-particle 100))
    (set! *scml* (scml-load "monster/Example.SCML"))
    (set! *anim* (animation (entity *scml* "0") "Idle"))))

(define *speed* 50)
(define *pos* (make-vect 320 100))

(define (update-view dt input)
  (let* ((cycles-for-anim (seconds->cycles (animation-length *anim*)))
         (anim-cycle (modulo (clock-time *game-clock*) cycles-for-anim))
         (anim-time (cycles->seconds anim-cycle))
	 (lr (scale-input (input-leftright input) dt))
	 (ud (scale-input (input-updown input) dt))
	 (temp (vect-add-into! *pos* *pos* (make-vect lr ud)))
         (sprite-list (add-animation #f *anim* anim-time
				     (vect-x *pos*) (vect-y *pos*))))

    ;(update-view-old dt)
    (spritelist-enqueue-for-screen! sprite-list)))
