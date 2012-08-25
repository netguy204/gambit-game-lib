(define-syntax when
  (syntax-rules ()
    ((when test . body) (if test (begin . body) #f))))

(define-syntax comment
  (syntax-rules ()
    ((comment . body) '())))

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
