(load "object")
(load "math")
(load "common")

(define (consume! predator resources resource? range-rect amount)
  (let loop ((objs (spatial-rect table range-rect))
             (amt amt))
    (cond
     ((null? objs) #f)
     ((resource? (car objs))
      )))
  (let* ((start (table-ref table resource #f))
         (end (and start (- start amt))))
    (if (and end (> end 0))
        (begin
          (table-set! table resource end)
          #t)
        #f)))

(define (consume-each! table range-rect resources)
  (let loop ((resources resources)
             (success #t))
    (if (null? resources)
        success
        (loop (cdr resources)
              (consume! table range-rect (caar resources) (cdar resources))))))

(define (update obj resources)
  (obj-invoke obj 'update resources))

(define (spawn obj)
  (obj-invoke obj 'spawn))

(define (within? this radius)
  (let ((tx (obj-field this 'x))
        (ty (obj-field this 'y)))
    (lambda (other)
      (let ((ox (obj-field other 'x))
            (oy (obj-field other 'y)))
        (> radius (dist tx ty ox oy))))))

(define (range-rect this)
  (let ((range (obj-field this 'range))
         (px (obj-field this 'x))
         (py (obj-field this 'y)))
    (rect-make-radius px py range)))

(define (resources-in-range this resources pred)
  (let* ((neighbors (spatial-rect resources (range-rect this)))
         (range? (within? this (obj-field this 'range))))
    (filter (lambda (o) (and (range? o) (pred o))) neighbors)))

;;; markov chain AI with decaying self-transition
(define (random-symbol symbols)
  (let loop ((value (random-real))
             (symbols symbols))
    (cond
     ((null? symbols) (error "symbols didn't cover [0,1]"))
     ((< value (cdar symbols)) (caar symbols))
     (#t (loop (- value (cdar symbols))
               (cdr symbols))))))

(define-structure timeline value)

(define timeline-make make-timeline)

(define (timeline-update timeline dt)
  (let* ((last (timeline-value timeline))
         (next (+ last dt)))
    (timeline-value-set! timeline next)
    next))

(define (markov-transition symbols state)
  (random-symbol (cdr (assoc state symbols))))

(define (exponential-decay time time-constant)
  (exp (/ time (- time-constant))))

(define idle-markov
  '((initial . ((hunting . 0.5)
                (sleeping . 0.5)))
    (hunting . ((sleeping . 0.5)
                (hunting . 0.1)
                (mating . 0.4)))
    (sleeping . ((sleeping . 0.6)
                 (hunting . 0.2)
                 (mating . 0.2)))
    (mating . ((sleeping . 0.8)
               (hunting . 0.1)
               (mating . 0.1)))))

(define (predator-update this resources)
  (let* ((prey? (obj-field this 'prey?)))
   (let ()
     (if (apply consume-each! resources prey)
         (list this (spawn this))
         '()))))

(define predator (obj-make fields: (list 'update predator-update
                                         'range 1
                                         'x 0
                                         'y 0)))

(define (fox-make chickens-per-cycle)
  (obj-make parent: predator
            fields: (list 'spawn (lambda (this) (fox-make chickens-per-cycle))
                          'prey (list 'chicken chickens-per-cycle))))

(define (chicken-make grain-per-cycle)
  (obj-make parent: predator
            fields: (list 'spawn (lambda (this) (chicken-make grain-per-cycle))
                          'prey (list 'grain grain-per-cycle))))

(define world (spatial-make 100))

(define (resources-seed resources)
  (define (random-point-rect)
    (rect-make-point (random-integer 100) (random-integer 100)))

  (repeatedly
   (lambda ()
     (spatial-update! resources
                      (fox-make (random-integer 3))
                      (random-point-rect)))
   (random-integer 50))

  (repeatedly
   (lambda ()
     (spatial-update! resources
                      (chicken-make (random-integer 3))
                      (random-point-rect)))
   (random-integer 50)))
