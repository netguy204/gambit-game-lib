(load "math")
(load "common")
(load "rect")
(load "spatial")
(load "sparrow")

(declare
 (standard-bindings)
 (extended-bindings))

(define-structure game-particle particle atlas img-name extra)

(define (game-particle-make particle atlas img-name #!optional (extra #f))
  (let* ((gp (make-game-particle particle atlas img-name extra)))

    gp))

(define (game-particle-w particle)
  (sparrow-width (game-particle-atlas particle) (game-particle-img-name particle)))

(define (game-particle-h particle)
  (sparrow-height (game-particle-atlas particle) (game-particle-img-name particle)))

(define (game-particle-width gp)
  (* (particle-s (game-particle-particle gp)) (game-particle-w gp)))

(define (game-particle-height gp)
  (* (particle-s (game-particle-particle gp)) (game-particle-h gp)))

(define (game-particle-r gp)
  (particle-r (game-particle-particle gp)))

(define (game-particle-dr gp)
  (particle-dr (game-particle-particle gp)))

(define (game-particle-x gp)
  (particle-x (game-particle-particle gp)))

(define (game-particle-y gp)
  (particle-y (game-particle-particle gp)))

(define (game-particle-cx gp)
  (/ (game-particle-w gp) 2.))

(define (game-particle-cy gp)
  (/ (game-particle-h gp) 2.))

(define (game-particle-t gp)
  (particle-t (game-particle-particle gp)))

(define (game-particle-dr-set! gp dr)
  (particle-dr-set! (game-particle-particle gp) dr))

(define (game-particle-s gp)
  (particle-s (game-particle-particle gp)))

(define (game-particle-rect-scaled gp s)
  (let* ((hw (* s (/ (game-particle-width gp) 2.)))
         (hh (* s (/ (game-particle-width gp) 2.)))
         (cx (* s (game-particle-x gp)))
         (cy (* s (game-particle-y gp))))
    (rect-make (- cx hw) (- cy hh) (+ cx hw) (+ cy hh))))

(define (game-particle-rect gp)
  (game-particle-rect-scaled gp 1))

(define (game-particle->sprite gp ocx ocy)
  (let ((sprite (frame/make-sprite))
        (w (game-particle-width gp))
        (h (game-particle-height gp)))
    (sprite-parms-set! sprite (sparrow-image (game-particle-atlas gp))
                       (game-particle-x gp) (game-particle-y gp)
                       (+ ocx 0.5)
                       (+ ocy 0.5)
                       (game-particle-t gp)
                       (* w (game-particle-s gp))
                       (* h (game-particle-s gp)))
    (sprite-sparrow-coords-set! sprite (game-particle-atlas gp)
                                (game-particle-img-name gp))
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

(define *base-volume* 1000)
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

(define *texture-atlas* #f)

(define (step-pretty-particles dt)
  (set! *particles*
    (mapcat (lambda (p)
              (filter not-null? (integrate-game-particle p dt)))
            *particles*)))

(define (spawn-smoke-particle source)
  (let* ((r (vect-copy (game-particle-r source)))
         (dr (vect-copy (game-particle-dr source)))
         (dr (vect-add-into! dr dr (random-vector 50 100)))
         (death-time (+ (seconds->cycles (/ (rand-in-range 500 3500) 1000.))
                        (clock-time *game-clock*))))
    (game-particle-make
     (particle-make
      r dr
      (rand-in-range 0 360) (rand-in-range -20 20)
      0.01 (* 0.5 (rand-in-range 1 4)))
     *texture-atlas*
     "smoke.png"
     (lambda (gp dt)
       (if (> (clock-time *game-clock*) death-time)
           '()
           (list (game-particle-integrate gp dt)))))))

(define (spawn-hulk-particle source img)
  (let* ((r (vect-copy (game-particle-r source)))
         (dr (vect-copy (game-particle-dr source)))
         (dr (vect-add-into! dr dr (random-vector 300 600)))
         (death-time (+ (seconds->cycles (/ (rand-in-range 500 6500) 1000.))
                        (clock-time *game-clock*))))
    (game-particle-make
     (particle-make
      r dr
      180 (rand-in-range -360 360)
      1 -1)
     *texture-atlas*
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
  (let* ((img-name "ship-right.png")
         (img-height (sparrow-height *texture-atlas* img-name))
         (img-width (sparrow-width *texture-atlas* img-name))
         (nrows (ceiling (/ *screen-height* img-height)))
         (shot-period (seconds->cycles (rand-in-range 1 5)))
         (next-shot (+ shot-period (clock-time *game-clock*))))

    (game-particle-make
     (particle-make (vect-make
                     (+ *screen-width* (/ (sparrow-width *texture-atlas* img-name) 2.))
                     (+ (/ img-height 2.)
                        (* img-height (rand-in-range 0 nrows))))
                    (vect-make (- (rand-in-range *enemy-speed*
                                                 (* 2 *enemy-speed*)))
                                 0)
                    180 0 1 0)
     *texture-atlas*
     img-name
     (lambda (gp dt)
       (if (> (clock-time *game-clock*) next-shot)
           (begin
             (set! *enemy-bullets*
                   (cons (spawn-bullet gp "enemy-bullet.png"
                                       *enemy-bullet-speed*)
                         *enemy-bullets*))
             (set! next-shot (+ (clock-time *game-clock*) shot-period))
	     (audio-enqueue (tone-make 600 (/ *base-volume* 2.) 0.05))))
       (game-particle-integrate gp dt)))))

(define (spawn-enemies n)
  (repeatedly spawn-enemy n))

(define (spawn-player)
  (let* ((img-name "hero.png"))
    (game-particle-make
     (particle-make (vect-make (sparrow-width *texture-atlas* img-name)
                               (/ *screen-height* 2.))
                    (vect-make 0 0)
                    0 0 1 0)
     *texture-atlas*
     img-name)))

(define (spawn-bullet shooter img speed)
  (let* ((sx (game-particle-x shooter))
         (sy (game-particle-y shooter))
         (ang (game-particle-t shooter))
         
         (dx (cos (degrees->radians ang)))
         (dy (sin (degrees->radians ang))))
    
    (game-particle-make
     (particle-make (vect-make sx sy)
                    (vect-make (* dx speed)
                               (* dy speed))
                    (rand-in-range 0 360) 500
                    0.25 0.5)
     *texture-atlas*
     img)))

(define (player-fire)
  (set! *player-bullets* (cons (spawn-bullet *player* "plasma.png"
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

(define *font-table* (make-table))

(define (build-font-table atlas)
  (let ((index 1))
    (for-each
     (lambda (char)
       (let* ((name (string-append (number->string index) ".png"))
              (rec (sparrow-record atlas name)))
        (table-set! *font-table* char rec)
        (set! index (+ index 1))))
     (string->list "abcdefghijklmnopqrstuvwxyz"))))

(define (char-rec char)
  (table-ref *font-table* (char-downcase char)))

(define (char->sprite char)
  (let ((sprite (frame/make-sprite))
        (rec (char-rec char)))
    (sprite-sparrow-record-set! sprite *texture-atlas* rec)
    sprite))

(define (char-width char)
  (%sparrow-entry-width (char-rec char)))

(define (char-height char)
  (%sparrow-entry-height (char-rec char)))

(define (string->spritelist string x y)
  (reduce
   (lambda (spritelist char)
     (if (eq? char #\space)
         (begin
           (set! x (+ x (char-width #\m)))
           spritelist)
         (let ((sprite (char->sprite char)))
           (sprite-x-set! sprite x)
           (sprite-y-set! sprite y)
           (set! x (+ x (char-width char)))
           (frame/spritelist-append spritelist sprite))))
   #f
   (string->list string)))

(define (ensure-resources)
  (set! *texture-atlas* (sparrow-load "spacer/images_default"))
  (build-font-table *texture-atlas*)

  (set! *player* (spawn-player))
  (set! *enemies* (spawn-enemies *initial-enemies*))
  (thread-start! (make-thread music)))

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
  (sinsampler-make (audio-current-sample) (seconds->samples duration)
                   freq amp 0))

(define (explosion-sound duration)
  (saw-samplers *base-volume* '(100 40) duration))

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
                                 "ship-right.png")))))

  (if (null? *enemy-bullets*)
      '()
      (with-collisions
       (list *player*) *enemy-bullets*

       (lambda (player bullet)
         (set! *enemy-bullets* (delete bullet *enemy-bullets*))
         (enqueue-all (explosion-sound 0.15))
         (add-pretty-particles! (spawn-hulk-particle
                                 player
                                 "hero.png"))))))

(define (random-spawn? num max-num prob)
  (let ((rand (rand-in-range 0 max-num))
        (thresh (* prob (- max-num num))))
    (< rand thresh)))

(define (spawn-and-terminate dt)
  ;; remove bullets that go off the right
  (for-each
   (lambda (bullet)
     (if (> (game-particle-x bullet) (+ (/ (game-particle-width bullet) 2.)
                                        *screen-width*))
         (set! *player-bullets* (delete bullet *player-bullets*))))
   *player-bullets*)

  ;; remove bullets that go off the left
  (for-each
   (lambda (bullet)
     (if (< (game-particle-x bullet) (- (/ (game-particle-width bullet) 2.)))
         (set! *enemy-bullets* (delete bullet *enemy-bullets*))))
   *enemy-bullets*)

  ;; remove enemies that go off the left
  (for-each
   (lambda (enemy)
     (if (< (game-particle-x enemy) (- (/ (game-particle-width enemy) 2.)))
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
    (sprite-width-set! sprite *screen-width*)
    (sprite-height-set! sprite *screen-height*)
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
      (game-particle-img-name-set! *player* "hero.png")
      (game-particle-img-name-set! *player* "hero-engines.png"))
  (spritelist-enqueue-for-screen!
   (game-particles->sprite-list (cons *player* *player-bullets*)))

  ;; draw a letter
  (spritelist-enqueue-for-screen! (string->spritelist "Hello World" 100 100)))

(define *player-fire-repeater* (repeating-latch-make 0.2 #f))

(define (update-view dt input)
  (let ((updown (input-updown input))
        (leftright (input-leftright input))
        (action1 (input-action1 input)))
    (game-particle-dr-set!
     *player*
     (vect-make (* *player-speed* leftright)
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
  (set! update-view (lambda (dt input) '()))
  (set! music (lambda () '())))

(define (enqueue-all samplers)
  (for-each audio-enqueue samplers))

(define (samplers constructor amp freqs duration mix start-sample)
  (let* ((duration (if mix duration (/ duration (length freqs)))))
    (map
     (lambda (f)
       (let ((sampler (constructor start-sample
                                   (seconds->samples duration)
                                   f amp 0)))
         (if (not mix)
           (set! start-sample (+ start-sample (seconds->samples duration))))
         sampler))
     freqs)))

(define (sin-samplers amp freqs duration #!key (mix #t) (start (audio-current-sample)))
  (samplers sinsampler-make amp freqs duration mix start))

(define (saw-samplers amp freqs duration #!key (mix #t) (start (audio-current-sample)))
  (samplers sawsampler-make amp freqs duration mix start))

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

;;; scales (derived from notes)
(define +ionian+ (list 2 2 1 2 2 2 1))
(define +locrian+ (list 1 2 2 1 2 2 2))
(define +aeolian+ (list 2 1 2 2 1 2 2))

(define (index-sequentially offsets lst)
  (let ((idx 0)
        (lst-len (length lst)))
    (map
     (lambda (offset)
       (set! idx (+ idx offset))
       (* (ceiling (/ (+ 1 idx) lst-len))
          (list-ref lst (modulo idx lst-len))))
     offsets)))


(define (scale type center)
  (let ((idx (index-of center +notes+))
        (notes (length +notes+)))
    (index-sequentially (cons idx type)
                        +notes+)))

(define +c-major-scale+ (scale +ionian+ +c+))

;;; chord forms (derived from scales)
(define +triad+ (list 0 2 2))
(define +diminished-triad+ (list 0 1 2))
(define +7th+ (list 0 2 2 2))
(define +6th+ (list 0 2 2 1))
(define +sus2+ (list 0 1 3))
(define +sus4+ (list 0 3 1))

(define (chord scale-type chord-type center)
  (let ((scale-notes (scale scale-type center)))
    (index-sequentially chord-type scale-notes)))

(define +c-major+ (chord +ionian+ +triad+ +c+))

(define (enqueue-chord-sequence scale-type chord-type amp freqs duration
                                #!optional (sampler sin-samplers))
  (thread-start!
   (make-thread
    (lambda ()
      (for-each
       (lambda (note)
         (enqueue-all
          (sampler amp (chord scale-type chord-type note) duration))
         (thread-sleep! duration))
       freqs)))))

(define (enqueue-arpeggio-sequence scale-type chord-type amp freqs duration
                                   #!optional (sampler sin-samplers))
  (thread-start!
   (make-thread
    (lambda ()
      (for-each
       (lambda (note)
         (enqueue-all
          (sampler amp (chord scale-type chord-type note) duration mix: #f))
         (thread-sleep! duration))
       freqs)))))

(define +last-silent-sample+ #f)
(define (play freqs duration)
  (if (not +last-silent-sample+) (set! +last-silent-sample+ (audio-current-sample)))
  (if (and (> +last-silent-sample+ 0)
           (< (audio-current-sample) 0))
      ;; this is an integer wraparound situation. we'll get an audio mishap
      (set! +last-silent-sample+ (audio-current-sample)))

  ;; wait till the next silence is two durations
  (let loop ((delta (- +last-silent-sample+ (audio-current-sample))))
    (if (> delta (seconds->samples 10))
        (begin
          (thread-sleep! 0.1)
          (loop (- +last-silent-sample+ (audio-current-sample))))))

  (enqueue-all (sin-samplers 1000 freqs duration start: +last-silent-sample+))
  (set! +last-silent-sample+ (+ +last-silent-sample+ (seconds->samples duration))))

(define (bpm->seconds n notes-per-beat)
  (/ n (* notes-per-beat 60.)))

(define (progression base)
  (let ((duration (bpm->seconds 120 4)))

    (play (list (* base 4/3) base) duration)
    (play (list (* base 3/3) (* 2 base)) duration)
    (play (list (* base 4/3) (* 2 base)) (/ duration 2.))
    (play (list (* base 4/3) base) (/ duration 2.))

    (play (list (* base 1/3) (* 1 base)) duration)

    (play (list (* 1/3 base) (* 1 base)) (/ duration 2.))
    (play (list (* 1/3 base) (* 2 base)) (/ duration 2.))

    (play (list (* base 1/3) base) duration)

    (play (list (* 1/3 base) (* 2 base)) (/ duration 3.))
    (play (list (* 2/3 base) (* 1 base)) (/ duration 3.))
    (play (list (* 2/3 base) (* 2 base)) (/ duration 3.))

    (play (list (* base 2/3) base) duration)

    (play (list (* 1/3 base) (* 2 base)) (/ duration 3.))
    (play (list (* 1/3 base) (* 1 base)) (/ duration 3.))
    (play (list (* 2/3 base) (* 1 base)) (/ duration 3.))

    (play (list (* base 4/3) (* 2 base)) duration)

    (play (list (* 1/3 base) (* 2 base)) (/ duration 2.))
    (play (list (* 1/3 base) (* 1 base)) (/ duration 2.))))

(define (music)
  (progression 100.)
  (progression 150.)
  (progression 75.)
  (progression 50.)
  (music))

#|
(define (music) '())
(thread-start! (make-thread music))
|#

;;; examples
#|
(enqueue-all (saw-samplers 6000 '(200 50) 2))
(enqueue-all (saw-samplers 6000 '(180 50) 2))
(enqueue-all (saw-samplers 6000 '(200 150) 2))
(enqueue-all (saw-samplers 10000 '(200 150) 1 mix: #f))
(enqueue-all (sin-samplers 10000
                           (list +c+ +d+ +e+
                                 +c+ +d+ +e+
                                 +c+ +d+ +e+ +d+ +c+ +d+ +c+)
                           2 mix: #f))

(enqueue-arpeggio-sequence +ionian+ +sus2+ 10000
                        (list +c+ +d+ +e+
                              +c+ +d+ +e+
                              +c+ +d+ +e+ +d+ +c+ +d+ +c+)
                        0.6)
|#
