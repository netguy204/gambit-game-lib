(define (tkey->sprite tkey ox oy)
  (let* ((image (image-load (tkey-name tkey)))
         (sprite (frame/make-sprite))
         (h (image-height image))
         (w (image-width image))
         (px-img-offset (* (tkey-cx tkey) w))
         (py-img-offset (* (tkey-cy tkey) h))
         (piv-x (+ ox (tkey-x tkey)))
         (piv-y (+ oy (tkey-y tkey))))

    (sprite-resource-set! sprite image)
    (sprite-x-set! sprite piv-x)
    (sprite-y-set! sprite piv-y)
    (sprite-origin-x-set! sprite px-img-offset)
    (sprite-origin-y-set! sprite py-img-offset)

    (sprite-angle-set! sprite (tkey-angle tkey))
    sprite))

(define (add-animation sprite-list anim time ox oy)
  (reduce (lambda (sprite-list tkey)
            (frame/spritelist-append sprite-list
                                     (tkey->sprite tkey ox oy)))
          #f
          (reverse (interp-anim anim time))))

