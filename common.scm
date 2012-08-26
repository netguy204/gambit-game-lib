
(define (filter pred lst)
  (let loop ((result '())
             (lst lst))
    (if (null? lst)
        (reverse result)
        (if (pred (car lst))
            (loop (cons (car lst) result)
                  (cdr lst))
            (loop result
                  (cdr lst))))))

(define (remove-if pred lst)
  (filter (lambda (x) (not (pred x))) lst))

(define (some? pred lst)
  (let loop ((lst lst))
    (cond
     ((null? lst) #f)
     ((pred (car lst)) #t)
     (#t (loop (cdr lst))))))

(define (repeatedly fn n)
  (let loop ((result '())
             (n n))
    (if (> n 0)
        (loop (cons (fn) result)
              (- n 1))
        (reverse result))))

(define (identity o) o)

(define (concat lsts)
  (let ((result '()))
    (let loop-lsts ((lsts lsts))
      (if (null? lsts)
          (reverse result)
          (let loop ((lst (car lsts)))
            (if (null? lst)
                (loop-lsts (cdr lsts))
                (begin
                  (set! result (cons (car lst) result))
                  (loop (cdr lst)))))))))

(define (mapcat fn lst)
  (concat (map fn lst)))

(define (reduce fn init lst)
  (let loop ((result init)
             (lst lst))
    (if (null? lst) result
        (loop (fn result (car lst))
              (cdr lst)))))
