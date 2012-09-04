(load "math")
(load "common")
(load "rect")
(load "spatial")

(define-structure particle r dr t dt s ds)

(define (particle-x p)
  (vect-x (particle-r p)))

(define (particle-y p)
  (vect-y (particle-r p)))

(define (particle-integrate p dt)
  (vect-add-into! (particle-r p)
                  (particle-r p)
                  (vect-scale (particle-dr p) dt))
  (particle-t-set! p (+ (particle-t p)
                        (* (particle-dt p) dt)))
  (particle-s-set! p (max 0 (+ (particle-s p)
                              (* (particle-ds p) dt)))))

(define (rand-in-range min max)
  (+ min (random-integer (- max min))))

(define (random-vector maxx maxy)
  (make-vect (rand-in-range (- maxx) maxx)
             (rand-in-range (- maxy) maxy)))

(define (random-particle img)
  (let ((img-w (image-width img))
        (img-h (image-height img)))
    (make-particle (make-vect (random-integer (- 640 img-w))
                              (random-integer (- 480 img-h)))
                   (make-vect (rand-in-range -100 100)
                              (rand-in-range -100 100))
                   (rand-in-range 0 360)
                   (rand-in-range 100 1000)
                   1
                   0)))

(define *anim* #f)
(define *scml* #f)

(define (sign val)
  (cond
   ((= val 0) 0)
   ((> val 0) -1)
   ((< val 0) 1)
   (#t (error "bad input " val))))

(define-structure game-particle particle img-name extra)

(define (game-particle-img gp)
  (image-load (game-particle-img-name gp)))

(define (game-particle-r gp)
  (particle-r (game-particle-particle gp)))

(define (game-particle-dr gp)
  (particle-dr (game-particle-particle gp)))

(define (game-particle-x gp)
  (particle-x (game-particle-particle gp)))

(define (game-particle-y gp)
  (particle-y (game-particle-particle gp)))

(define (game-particle-cx gp)
  (let ((img (game-particle-img gp)))
    (/ (image-width img) 2)))

(define (game-particle-cy gp)
  (let ((img (game-particle-img gp)))
    (/ (image-height img) 2)))

(define (game-particle-t gp)
  (particle-t (game-particle-particle gp)))

(define (game-particle-dr-set! gp dr)
  (particle-dr-set! (game-particle-particle gp) dr))

(define (game-particle-s gp)
  (particle-s (game-particle-particle gp)))

(define (game-particle-rect-scaled gp s)
  (let* ((img (game-particle-img gp))
         (hw (* s (/ (image-width img) 2)))
         (hh (* s (/ (image-height img) 2)))
         (p (game-particle-particle gp))
         (cx (* s (particle-x p)))
         (cy (* s (particle-y p))))
    (rect-make (- cx hw) (- cy hh) (+ cx hw) (+ cy hh))))

(define (game-particle-rect gp)
  (game-particle-rect-scaled gp 1))

(define (game-particle->sprite gp ocx ocy)
  (let ((sprite (frame/make-sprite)))
    (sprite-parms-set! sprite (game-particle-img gp)
                       (game-particle-x gp) (game-particle-y gp)
                       (+ ocx (game-particle-cx gp))
                       (+ ocy (game-particle-cy gp))
                       (game-particle-t gp) (game-particle-s gp))
    sprite))

(define (game-particles->sprite-list gps #!optional (ocx 0) (ocy 0))
  (reduce (lambda (sprite-list gp)
            (frame/spritelist-append
             sprite-list
             (game-particle->sprite gp ocx ocy)))
          #f
          gps))

(define (game-particle-integrate gp dt)
  (particle-integrate (game-particle-particle gp) dt)
  gp)

(define *player* '())
(define *player-bullets* '())
(define *enemy-bullets* '())
(define *enemies* '())
(define *enemy-bullets* '())
(define *particles* '())

(define *enemy-speed* 50)
(define *player-speed* 600)
(define *player-bullet-speed* 1200)
(define *enemy-bullet-speed* 400)
(define *initial-enemies* 1)
(define *max-enemies* 10)

(define (step-pretty-particles dt)
  (set! *particles*
    (mapcat (lambda (p)
              (filter not-null? (integrate-game-particle p dt)))
            *particles*)))

(define (spawn-smoke-particle source)
  (let* ((r (vect-copy (game-particle-r source)))
         (dr (vect-copy (game-particle-dr source)))
         (dr (vect-add-into! dr dr (random-vector 50 100)))
         (death-time (+ (seconds->cycles (/ (rand-in-range 500 3500) 1000))
                        (clock-time *game-clock*))))
    (make-game-particle
     (make-particle
      r dr
      (rand-in-range 0 360) (rand-in-range -20 20)
      0.01 (* 0.5 (rand-in-range 1 4)))
     "spacer/smoke.png"
     (lambda (gp dt)
       (if (> (clock-time *game-clock*) death-time)
           '()
           (list (game-particle-integrate gp dt)))))))

(define (spawn-hulk-particle source img)
  (let* ((r (vect-copy (game-particle-r source)))
         (dr (vect-copy (game-particle-dr source)))
         (dr (vect-add-into! dr dr (random-vector 300 600)))
         (death-time (+ (seconds->cycles (/ (rand-in-range 500 6500) 1000))
                        (clock-time *game-clock*))))
    (make-game-particle
     (make-particle
      r dr
      180 (rand-in-range -360 360)
      1 -1)
     img
     (lambda (gp dt)
       (if (> (clock-time *game-clock*) death-time)
           '()
           (list (game-particle-integrate gp dt)))))))

(define (add-pretty-particles! p)
  (if (list? p)
      (set! *particles* (append p *particles*))
      (set! *particles* (cons p *particles*))))

(define (spawn-enemy)
  (let* ((img-name "spacer/ship-right.png")
         (img (image-load img-name))
         (nrows (ceiling (/ *screen-height* (image-height img))))
         (shot-period (seconds->cycles (rand-in-range 1 5)))
         (next-shot (+ shot-period (clock-time *game-clock*))))

    (make-game-particle
     (make-particle (make-vect
                     (+ *screen-width* (/ (image-width img) 2))
                     (+ (/ (image-height img) 2)
                        (* (image-height img) (rand-in-range 0 nrows))))
                    (make-vect (- (rand-in-range *enemy-speed*
                                                 (* 2 *enemy-speed*)))
                                 0)
                    180 0 1 0)
     img-name
     (lambda (gp dt)
       (if (> (clock-time *game-clock*) next-shot)
           (begin
             (set! *enemy-bullets*
                   (cons (spawn-bullet gp "spacer/enemy-bullet.png"
                                       *enemy-bullet-speed*)
                         *enemy-bullets*))
             (set! next-shot (+ (clock-time *game-clock*) shot-period))))
       (game-particle-integrate gp dt)))))

(define (spawn-enemies n)
  (repeatedly spawn-enemy n))

(define (spawn-player)
  (let* ((img-name "spacer/hero.png")
         (img (image-load img-name)))
    (make-game-particle
     (make-particle (make-vect (image-width img)
                                 (/ *screen-height* 2))
                    (make-vect 0 0)
                    0 0 1 0)
     img-name
     #f)))

(define pi 3.141592654)

(define (degrees->radians deg)
  (/ (* pi deg) 180))

(define (radians->degrees rad)
  (/ (* rad 180) pi))

(define (spawn-bullet shooter img speed)
  (let* ((sx (game-particle-x shooter))
         (sy (game-particle-y shooter))
         (ang (game-particle-t shooter))
         
         (dx (cos (degrees->radians ang)))
         (dy (sin (degrees->radians ang))))
    
    (make-game-particle
     (make-particle (make-vect sx sy)
                    (make-vect (* dx speed)
                               (* dy speed))
                    (rand-in-range 0 360) 500
                    0.25 1)
     img
     #f)))

(define (player-fire)
  (set! *player-bullets* (cons (spawn-bullet *player* "spacer/plasma.png"
                                             *player-bullet-speed*)
                               *player-bullets*)))

(define-structure repeating-latch period latch-value last-state last-time)

(define (repeating-latch-make period latch-value)
  (make-repeating-latch period latch-value 0 0))

(define (repeating-latch-state latch input-state)
  (if (not (eq? (repeating-latch-last-state latch) input-state))
      (begin
        (repeating-latch-last-state-set! latch input-state)
        (repeating-latch-last-time-set! latch (clock-time *game-clock*))
        input-state)

      (if (> (cycles->seconds (- (clock-time *game-clock*)
                                 (repeating-latch-last-time latch)))
             (repeating-latch-period latch))
          (begin
            (repeating-latch-last-time-set! latch (clock-time *game-clock*))
            input-state)
          (repeating-latch-latch-value latch))))

(define (ensure-resources)
  (set! *player* (spawn-player))
  (set! *enemies* (spawn-enemies *initial-enemies*)))

(define (integrate-game-particle gp dt)
  (let ((extra (game-particle-extra gp)))
    (if extra
        (extra gp dt)
        (game-particle-integrate gp dt))))

(define (integrate-objects dt)
  (let ((integrate (lambda (item) (integrate-game-particle item dt))))
    (step-pretty-particles dt)
    (for-each integrate (list *player*))
    (for-each integrate *player-bullets*)
    (for-each integrate *enemy-bullets*)
    (for-each integrate *enemies*)
    (for-each integrate *enemy-bullets*)))

(define (with-collisions as bs fn)
  (for-each (lambda (a)
              (for-each (lambda (b)
                          (if (rect-intersect (game-particle-rect a)
                                              (game-particle-rect b))
                              (fn a b)))
                        bs))
            as))

(define *spatial-scale-factor* (/ 256.))

(define (game-particle-spatial-rect g)
  (game-particle-rect-scaled g *spatial-scale-factor*))

(define (with-spatial-collisions spatial bs fn)
  (for-each (lambda (b)
              (let* ((rect (game-particle-spatial-rect b))
                     (as (spatial-rect spatial rect)))
                (if (not (null? as))
                    (with-collisions as (list b) fn))))
            bs))

(define (game-objects->spatial gs)
  (reduce (lambda (spatial g)
            (spatial-update! spatial g (game-particle-spatial-rect g))
            spatial)
          (spatial-make (* *screen-width* *spatial-scale-factor*))
          gs))

(define (handle-collisions)
  (if (or (null? *player-bullets*)
          (null? *enemies*))
      '()
      (with-spatial-collisions
       (game-objects->spatial *player-bullets*)
       *enemies*

       (lambda (bullet enemy)
         (set! *player-bullets* (delete bullet *player-bullets*))
         (set! *enemies* (delete enemy *enemies*))
         (add-pretty-particles! (spawn-smoke-particle enemy))
         (add-pretty-particles! (spawn-hulk-particle
                                 bullet
                                 "spacer/ship-right.png"))

         (audio-enqueue
          (stepsampler-make
           (sinsampler-make 300 8000 0)
           (audio-current-sample) (seconds->samples 0.1))))))

  (if (null? *enemy-bullets*)
      '()
      (with-collisions
       (list *player*) *enemy-bullets*

       (lambda (player bullet)
         (set! *enemy-bullets* (delete bullet *enemy-bullets*))
         (add-pretty-particles! (spawn-hulk-particle
                                 player
                                 "spacer/hero.png"))))))

(define (random-spawn? num max-num prob)
  (let ((rand (rand-in-range 0 max-num))
        (thresh (* prob (- max-num num))))
    (< rand thresh)))

(define (spawn-and-terminate dt)
  ;; remove bullets that go off the right
  (for-each
   (lambda (bullet)
     (if (> (game-particle-x bullet) (+ (/ (image-width
                                            (game-particle-img bullet)) 2)
                                        *screen-width*))
         (set! *player-bullets* (delete bullet *player-bullets*))))
   *player-bullets*)

  ;; remove bullets that go off the left
  (for-each
   (lambda (bullet)
     (if (< (game-particle-x bullet) (- (/ (image-width
                                            (game-particle-img bullet)) 2)))
         (set! *enemy-bullets* (delete bullet *enemy-bullets*))))
   *enemy-bullets*)

  ;; remove enemies that go off the left
  (for-each
   (lambda (enemy)
     (if (< (game-particle-x enemy) (- (/ (image-width
                                           (game-particle-img enemy)) 2)))
         (set! *enemies* (delete enemy *enemies*))))
   *enemies*)

  ;; spawn new enemies
  (if (random-spawn? (length *enemies*) *max-enemies* 0.1)
      (set! *enemies* (cons (spawn-enemy) *enemies*))))

(define (stars-spritelist)
  (let ((stars (image-load "spacer/night-sky-stars.jpg"))
        (sprite (frame/make-sprite)))
    (sprite-resource-set! sprite stars)
    (sprite-x-set! sprite 0)
    (sprite-y-set! sprite 0)
    (frame/spritelist-append #f sprite)))

(define (render input)
  (spritelist-enqueue-for-screen!
   (stars-spritelist))

  (spritelist-enqueue-for-screen!
   (game-particles->sprite-list *particles*))

  (spritelist-enqueue-for-screen!
   (game-particles->sprite-list *enemies*))

  (spritelist-enqueue-for-screen!
   (game-particles->sprite-list *enemy-bullets*))

  (if (and (= 0 (input-leftright input))
           (= 0 (input-updown input)))
      (game-particle-img-name-set! *player* "spacer/hero.png")
      (game-particle-img-name-set! *player* "spacer/hero-engines.png"))
  (spritelist-enqueue-for-screen!
   (game-particles->sprite-list (cons *player* *player-bullets*))))

(define *player-fire-repeater* (repeating-latch-make 0.2 #f))

(define (update-view dt input)
  (let ((updown (input-updown input))
        (leftright (input-leftright input))
        (action1 (input-action1 input)))
    (game-particle-dr-set!
     *player*
     (make-vect (* *player-speed* leftright)
                (* *player-speed* updown)))

    (if (repeating-latch-state *player-fire-repeater*  action1)
        (begin
          (audio-enqueue
           (stepsampler-make
            (sinsampler-make 400 8000 0)
            (audio-current-sample) (seconds->samples 0.1)))
         (player-fire))))

  (integrate-objects dt)
  (spawn-and-terminate dt)
  (handle-collisions)
  (render input))
