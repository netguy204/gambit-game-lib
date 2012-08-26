(load "xml2")
(load "common")

(define (resource-id-make folder-id file-id)
  (cons folder-id file-id))

(define-structure scml resource-names)

(define (folders-markup scml)
  (filter (sml:node-named? "folder") (sml:children scml)))

(define (entities-markup scml)
  (filter (sml:node-named? "entity") (sml:children scml)))

(define (entity->animations-markup entity)
  (filter (sml:node-named? "animation") (sml:children entity)))

(define (animation->mainline-markup animation)
  (car (filter (sml:node-named? "mainline") (sml:children animation))))

(define (animation->timelines-markup animation)
  (filter (sml:node-named? "timeline") (sml:children animation)))

(define (timeline->keys-markup timeline)
  (filter (sml:node-named? "key") (sml:children timeline)))

(define-structure animation id name length)
(define-structure timeline time-keys id)

(define (timeline-parse timeline-markup)
  )
(define (extract-files scml)
  (mapcat (lambda (folder)
            (map (lambda (file)
                   (cons (resource-id-make
                          (sml:attr folder "id")
                          (sml:attr file "id"))
                         (sml:attr file "name")))
                 (sml:children folder)))
          (folders-markup scml)))

(define (scml-load filename)
  (let* ((doc (xml:parse-file filename))
         (node (if doc
                   (xml:root-element doc)
                   (error "failed to load " filename)))
         (scml (xml->sml node)))))


(comment
 (define timeline (car (animation->timelines-markup (car (entity->animations-markup (car (entities-markup sml)))))))

 )
