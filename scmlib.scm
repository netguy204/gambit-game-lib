(load "math")
(load "common")
(load "rect")

(define-structure particle r dr t tr)

(define (particle-x p)
  (vect-x (particle-r p)))

(define (particle-y p)
  (vect-y (particle-r p)))

(define (particle-integrate p dt)
  (vect-add-into! (particle-r p)
                  (particle-r p)
                  (vect-scale (particle-dr p) dt))
  (particle-t-set! p (+ (particle-t p)
                        (* (particle-tr p) dt))))

(define (rand-in-range min max)
  (+ min (random-integer (- max min))))

(define (random-particle img)
  (let ((img-w (image-width img))
        (img-h (image-height img)))
    (make-particle (make-vect (random-integer (- 640 img-w))
                              (random-integer (- 480 img-h)))
                   (make-vect (rand-in-range -100 100)
                              (rand-in-range -100 100))
                   (rand-in-range 0 360)
                   (rand-in-range 100 1000))))

(define *anim* #f)
(define *scml* #f)

(define (scale-input val dt)
  (cond
   ((= val 0) 0)
   ((> val 0) (* dt *speed*))
   ((< val 0) (- (* dt *speed*)))
   (#t (error "bad input " val))))

(define-structure game-particle particle img-name)

(define (game-particle-img gp)
  (image-load (game-particle-img-name gp)))

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

(define (game-particle-rect gp)
  (let* ((img (game-particle-img gp))
         (hw (/ (image-width img) 2))
         (hh (/ (image-height img) 2))
         (p (game-particle-particle gp))
         (cx (particle-x p))
         (cy (particle-y p)))
    (rect-make (- cx hw) (- cy hh) (+ cx hw) (+ cy hh))))

(define (game-particle->sprite gp)
  (let ((sprite (frame/make-sprite)))
    (sprite-resource-set! sprite (game-particle-img gp))
    (sprite-x-set! sprite (game-particle-x gp))
    (sprite-y-set! sprite (game-particle-y gp))
    (sprite-origin-x-set! sprite (game-particle-cx gp))
    (sprite-origin-y-set! sprite (game-particle-cy gp))
    (sprite-angle-set! sprite (game-particle-t gp))
    sprite))

(define (game-particles->sprite-list gps)
  (reduce (lambda (sprite-list gp)
            (frame/spritelist-append
             sprite-list
             (game-particle->sprite gp)))
          #f
          gps))

(define *player* '())
(define *player-bullets* '())
(define *enemies* '())
(define *enemy-bullets* '())

(define *enemy-speed* 50)
(define *player-speed* 300)
(define *bullet-speed* 1200)
(define *initial-enemies* 5)
(define *max-enemies* 30)

(define (spawn-enemy)
  (let* ((img-name "spacer/ship-right.png")
         (img (image-load img-name)))
    (make-game-particle
     (make-particle (make-vect
                     (- *screen-width* (image-width img))
                     (rand-in-range 0 (- *screen-height* (image-height img))))
                    (make-vect (- *enemy-speed*)
                                 0)
                    180
                    0)
     img-name)))

(define (spawn-enemies n)
  (repeatedly spawn-enemy n))

(define (spawn-player)
  (let* ((img-name "spacer/ship-right.png")
         (img (image-load img-name)))
    (make-game-particle
     (make-particle (make-vect (image-width img)
                                 (/ *screen-height* 2))
                    (make-vect 0 0)
                    0
                    0)
     img-name)))

(define pi 3.141592654)

(define (degrees->radians deg)
  (/ (* pi deg) 180))

(define (radians->degrees rad)
  (/ (* rad 180) pi))

(define (spawn-bullet shooter)
  (let* ((sx (game-particle-x shooter))
         (sy (game-particle-y shooter))
         (ang (game-particle-t shooter))
         
         (dx (cos (degrees->radians ang)))
         (dy (sin (degrees->radians ang))))
    
    (make-game-particle
     (make-particle (make-vect sx sy)
                    (make-vect (* dx *bullet-speed*)
                               (* dy *bullet-speed*))
                    ang
                    500)
     "spacer/plasma.png")))

(define (player-fire)
  (set! *player-bullets* (cons (spawn-bullet *player*) *player-bullets*)))

(define (ensure-resources)
  (set! *player* (spawn-player))
  (set! *enemies* (spawn-enemies *initial-enemies*)))

(define (integrate-game-particle gp dt)
  (particle-integrate (game-particle-particle gp) dt))

(define (integrate-objects dt)
  (let ((integrate (lambda (item) (integrate-game-particle item dt))))
    (for-each integrate (list *player*))
    (for-each integrate *player-bullets*)
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

(define (handle-collisions)
  (with-collisions *player-bullets* *enemies*
    (lambda (bullet enemy)
      (set! *player-bullets* (delete bullet *player-bullets*))
      (set! *enemies* (delete enemy *enemies*)))))

(define (random-spawn? num max-num prob)
  (let ((rand (rand-in-range 0 max-num))
        (thresh (* prob (- max-num num))))
    (< rand thresh)))

(define (spawn-and-terminate dt)
  (for-each
   (lambda (bullet)
     (if (> (game-particle-x bullet) *screen-width*)
         (set! *player-bullets* (delete bullet *player-bullets*))))

   *player-bullets*)

  (if (random-spawn? (length *enemies*) *max-enemies* 0.3)
      (set! *enemies* (cons (spawn-enemy) *enemies*))))

(define (render)
  (spritelist-enqueue-for-screen!
   (game-particles->sprite-list *enemies*))

  (spritelist-enqueue-for-screen!
   (game-particles->sprite-list (cons *player* *player-bullets*))))

(define (update-view dt input)
  (let ((updown (input-updown input))
        (leftright (input-leftright input))
        (action1 (input-action1 input)))
    (game-particle-dr-set!
     *player*
     (make-vect (* *player-speed* leftright)
                (* *player-speed* updown)))

    (if action1
        (player-fire)))

  (integrate-objects dt)
  (spawn-and-terminate dt)
  (handle-collisions)
  (render))
