(load "math")
(load "common")
(load "rect")
(load "spatial")

(define-structure game-particle particle img-name extra w h)

(define (game-particle-make particle img-name #!optional (extra #f))
  (let* ((gp (make-game-particle particle img-name extra 0 0))
	 (img (game-particle-img gp)))
    (game-particle-w-set! gp (image-width img))
    (game-particle-h-set! gp (image-height img))
    gp))

(define (game-particle-width gp)
  (* (particle-s (game-particle-particle gp)) (game-particle-w gp)))

(define (game-particle-height gp)
  (* (particle-s (game-particle-particle gp)) (game-particle-h gp)))

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
  (/ (game-particle-w gp) 2))

(define (game-particle-cy gp)
  (/ (game-particle-h gp) 2))

(define (game-particle-t gp)
  (particle-t (game-particle-particle gp)))

(define (game-particle-dr-set! gp dr)
  (particle-dr-set! (game-particle-particle gp) dr))

(define (game-particle-s gp)
  (particle-s (game-particle-particle gp)))

(define (game-particle-rect-scaled gp s)
  (let* ((hw (* s (/ (game-particle-width gp) 2)))
         (hh (* s (/ (game-particle-width gp) 2)))
         (cx (* s (game-particle-x gp)))
         (cy (* s (game-particle-y gp))))
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

(define *base-volume* 8000)
(define *player* '())
(define *player-bullets* '())
(define *enemy-bullets* '())
(define *enemies* '())
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
    (game-particle-make
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
    (game-particle-make
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

    (game-particle-make
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
             (set! next-shot (+ (clock-time *game-clock*) shot-period))
	     (audio-enqueue (tone-make 600 *base-volume* 0.1))))
       (game-particle-integrate gp dt)))))

(define (spawn-enemies n)
  (repeatedly spawn-enemy n))

(define (spawn-player)
  (let* ((img-name "spacer/hero.png")
         (img (image-load img-name)))
    (game-particle-make
     (make-particle (make-vect (image-width img)
                                 (/ *screen-height* 2))
                    (make-vect 0 0)
                    0 0 1 0)
     img-name)))

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
    
    (game-particle-make
     (make-particle (make-vect sx sy)
                    (make-vect (* dx speed)
                               (* dy speed))
                    (rand-in-range 0 360) 500
                    0.25 1)
     img)))

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

(define (tone-make freq amp duration)
  (stepsampler-make
   (sinsampler-make freq amp 0)
   (audio-current-sample) (seconds->samples duration)))

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

         (audio-enqueue (tone-make 100 *base-volume* 0.1)))))

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
     (if (> (game-particle-x bullet) (+ (/ (game-particle-width bullet) 2)
                                        *screen-width*))
         (set! *player-bullets* (delete bullet *player-bullets*))))
   *player-bullets*)

  ;; remove bullets that go off the left
  (for-each
   (lambda (bullet)
     (if (< (game-particle-x bullet) (- (/ (game-particle-width bullet) 2)))
         (set! *enemy-bullets* (delete bullet *enemy-bullets*))))
   *enemy-bullets*)

  ;; remove enemies that go off the left
  (for-each
   (lambda (enemy)
     (if (< (game-particle-x enemy) (- (/ (game-particle-width enemy) 2)))
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
          (audio-enqueue (tone-make 300 (* 2 *base-volume*) 0.1))
	  (player-fire))))

  (integrate-objects dt)
  (spawn-and-terminate dt)
  (handle-collisions)
  (render input))

;;; tools for playing with audio

(define (disable-game)
  (set! update-view (lambda (dt input) '())))

(define (enqueue-all samplers)
  (for-each audio-enqueue samplers))

(define (mix samplers duration)
  (let ((start-sample (audio-current-sample)))
    (map
     (lambda (sampler)
       (stepsampler-make sampler start-sample
                         (seconds->samples duration)))
     samplers)))

(define (sequence-fixed-time samplers duration)
  (let ((start-sample (audio-current-sample)))
    (map (lambda (sampler)
           (let ((sampler (stepsampler-make sampler start-sample
                                            (seconds->samples duration))))
             (set! start-sample (+ start-sample (seconds->samples duration)))
             sampler))
         samplers)))

(define (samplers constructor amp freqs)
  (map (lambda (f) (constructor f amp 0)) freqs))

(define (sin-samplers amp freqs)
  (samplers sinsampler-make amp freqs))

(define (saw-samplers amp freqs)
  (samplers sawsampler-make amp freqs))

(define +c+ 261.6)
(define +c#+ 277.2)
(define +d+ 293.7)
(define +eb+ 311.1)
(define +e+ 329.6)
(define +f+ 349.2)
(define +f#+ 370.0)
(define +g+ 392.0)
(define +g#+ 415.3)
(define +a+ 440.0)
(define +bb+ 466.2)
(define +b+ 493.9)

(define +notes+ (list +c+ +c#+ +d+ +eb+ +e+ +f+ +f#+ +g+ +g#+ +a+ +bb+ +b+))
(define +ionian+ (list 2 2 1 2 2 2 1))
(define +locrian+ (list 1 2 2 1 2 2 2))
(define +aeolian+ (list 2 1 2 2 1 2 2))

(define (scale type center)
  (let ((idx (index-of center +notes+))
        (notes (length +notes+)))
    (map
     (lambda (offset)
       (set! idx (+ idx offset))
       (* (ceiling (/ (+ 1 idx) notes))
          (list-ref +notes+ (modulo idx notes))))
     (cons 0 type))))

(define +c-major+ (scale +ionian+ +c+))

;;; examples
#|
(enqueue-all (mix-fixed-time (saw-samplers 10000 '(20 15)) 2))
(enqueue-all (sequence-fixed-time (saw-samplers 10000 '(20 15)) 1))
(enqueue-all (sequence-fixed-time
              (sin-samplers 10000
                            (list +c+ +d+ +e+
                                  +c+ +d+ +e+
                                  +c+ +d+ +e+ +d+ +c+ +d+ +c+))
              .2))
|#
