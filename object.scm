;;; simple single-dispatch polymorphic message passing style object
;;; system

(define (plist->alist functions)
  (let loop ((result '())
             (functions functions))
    (if (null? functions)
        (reverse result)
        (loop (cons (cons (car functions) (cadr functions)) result)
              (cddr functions)))))

(define-structure %obj parent fields)

(define (%obj-make parent . fields)
  (make-%obj parent (plist->alist fields)))

(define (%obj-field obj field-name)
  (let ((field (assoc field-name (%obj-fields obj))))
    (if field (cdr field)
        (if (%obj-parent obj)
            (%obj-field (%obj-parent obj) field-name)
            (error "field does not exist: " field-name)))))

(define (%obj-field-set! obj field-name value)
  (let ((field (assoc field-name (%obj-fields obj))))
    (if field (set-cdr! field value)
        (%obj-fields-set! obj (cons (cons field-name value)
                                    (%obj-fields obj))))))

(define (%obj-invoke obj field . args)
  (let ((method (%obj-field obj field)))
    (apply method obj args)))

(define (.obj-field obj field)
  (%obj-field obj field))

(define (.obj-field-set! obj field value)
  (%obj-field-set! obj field value))

(define %obj-root (%obj-make #f
                             'field .obj-field
                             'field-set! .obj-field-set!))

(define (obj-make #!key (parent %obj-root) (fields '()))
  (apply %obj-make parent fields))

(define obj-field .obj-field)
(define obj-field-set! .obj-field-set!)
(define obj-invoke %obj-invoke)
