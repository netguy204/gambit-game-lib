;;; read xml files in the sparrow format
(load "xml2")

(define-structure %sparrow table image-file)
(define-structure %sparrow-entry x y width height)

(define (subtexture-markup sml)
  (filter (sml:node-named? "SubTexture") (sml:children sml)))

(define (sparrow-load base)
  (let* ((xml (string-append base ".xml"))
         (image-file (string-append base ".png"))
         (sparrow (sml:parse-file xml))
         (table (make-table)))

    (for-each
     (lambda (subtex)
       (let ((x (string->number (sml:attr subtex "x")))
             (y (string->number (sml:attr subtex "y")))
             (w (string->number (sml:attr subtex "width")))
             (h (string->number (sml:attr subtex "height")))
             (name (sml:attr subtex "name")))
        (table-set! table name (make-%sparrow-entry x y w h))))
     (subtexture-markup sparrow))

    (make-%sparrow table image-file)))

(define (sparrow-record sparrow name)
  (table-ref (%sparrow-table sparrow) name))

;;; now a link.scm specific approach
(define (sparrow-image sparrow)
  (image-load (%sparrow-image-file sparrow)))

(define (sparrow-width sparrow name)
  (%sparrow-entry-width (sparrow-record sparrow name)))

(define (sparrow-height sparrow name)
  (%sparrow-entry-height (sparrow-record sparrow name)))

(define (sparrow-x sparrow name)
  (%sparrow-entry-x (sparrow-record sparrow name)))

(define (sparrow-y sparrow name)
  (%sparrow-entry-y (sparrow-record sparrow name)))

(define (sparrow-texcoords sparrow name)
  (let* ((rec (sparrow-record sparrow name))
         (img (sparrow-image sparrow))
         (w (image-width img))
         (h (image-height img))
         (sp-x (%sparrow-entry-x rec))
         (sp-y (%sparrow-entry-y rec))
         (sp-w (%sparrow-entry-width rec))
         (sp-h (%sparrow-entry-height rec))
         (u0 (/ sp-x w))
         (u1 (/ (+ sp-x sp-w) w))
         (v0 (/ sp-y h))
         (v1 (/ (+ sp-y sp-h) h)))
    (rect-make (exact->inexact u0) (exact->inexact v0)
               (exact->inexact u1) (exact->inexact v1))))

(define (sprite-sparrow-coords-set! sprite sparrow name)
  (let* ((rec (sparrow-record sparrow name))
         (img (sparrow-image sparrow))
         (w (image-width img))
         (h (image-height img))
         (sp-x (%sparrow-entry-x rec))
         (sp-y (%sparrow-entry-y rec))
         (sp-w (%sparrow-entry-width rec))
         (sp-h (%sparrow-entry-height rec))
         (u0 (/ sp-x w))
         (u1 (/ (+ sp-x sp-w) w))
         (v0 (/ sp-y h))
         (v1 (/ (+ sp-y sp-h) h)))
    (sprite-coords-set! sprite u0 v0 u1 v1)))

