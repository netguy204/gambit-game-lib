;;; read xml files in the sparrow format
(load "xml2")

(define-structure %sparrow table image-file w h)
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

    (let* ((sparrow (make-%sparrow table image-file #f #f))
           (img (sparrow-image sparrow))
           (w (image-width img))
           (h (image-height img)))

      (%sparrow-w-set! sparrow w)
      (%sparrow-h-set! sparrow h)

      sparrow)))

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
         (w (%sparrow-w sparrow))
         (h (%sparrow-h sparrow))
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

(define (sprite-sparrow-record-coords-set! sprite sparrow rec)
  (let* ((w (%sparrow-w sparrow))
         (h (%sparrow-h sparrow))
         (sp-x (%sparrow-entry-x rec))
         (sp-y (%sparrow-entry-y rec))
         (sp-w (%sparrow-entry-width rec))
         (sp-h (%sparrow-entry-height rec))
         (u0 (/ sp-x w))
         (u1 (/ (+ sp-x sp-w) w))
         (v0 (/ (+ sp-y sp-h) h))
         (v1 (/ sp-y h)))

    (sprite-coords-set! sprite u0 v0 u1 v1)))

(define (sprite-sparrow-record-set! sprite sparrow rec)
  (sprite-sparrow-record-coords-set! sprite sparrow rec)
  (sprite-width-set! sprite (%sparrow-entry-width rec))
  (sprite-height-set! sprite (%sparrow-entry-height rec))
  (sprite-resource-set! sprite (sparrow-image sparrow)))

(define (sprite-sparrow-coords-set! sprite sparrow name)
  (let* ((rec (sparrow-record sparrow name)))
    (sprite-sparrow-record-coords-set! sprite sparrow rec)))

