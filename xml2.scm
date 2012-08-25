(c-declare #<<c-declare-end

#include <libxml/tree.h>
#include <libxml/parser.h>

c-declare-end
)

(load "common")

(define xml:parse-file
  (c-lambda (nonnull-char-string)
            (pointer "xmlDoc")
            "xmlParseFile"))

(define xml:get-root-element
  (c-lambda ((pointer "xmlDoc"))
            (pointer "xmlNode")
            "xmlDocGetRootElement"))

(define xml:free-doc
  (c-lambda ((pointer "xmlDoc"))
            void
            "xmlFreeDoc"))

(define (xml:null-safe arg fn)
  (if arg (fn arg)))

(define (xml:node-children node)
  (xml:null-safe node
    (c-lambda ((pointer "xmlNode"))
              (pointer "xmlNode")
              "___result_voidstar = ___arg1->children;")))

(define (xml:node-next node)
  (xml:null-safe node
    (c-lambda ((pointer "xmlNode"))
              (pointer "xmlNode")
              "___result_voidstar = ___arg1->next;")))

(define (xml:node-name node)
  (xml:null-safe node
    (c-lambda ((pointer "xmlNode"))
              nonnull-char-string
              "___result = ___arg1->name;")))

(define (xml:node-content node)
  (xml:null-safe node 
    (c-lambda ((pointer "xmlNode"))
              nonnull-char-string
              "___result = ___arg1->content;")))

(define (xml:node-properties node)
  (xml:null-safe node
    (c-lambda ((pointer "xmlNode"))
              (pointer "xmlAttribute")
              "___result_voidstar = ___arg1->properties;")))

(define (xml:node-type node)
  (xml:null-safe node
    (c-lambda ((pointer "xmlNode"))
              int
              "___result = ___arg1->type;")))

(define (xml:node-doc node)
  (xml:null-safe node
    (c-lambda ((pointer "xmlNode"))
              (pointer "xmlDoc")
              "___result = ___arg1->doc;")))

(define xml:node-list-string
  (c-lambda ((pointer "xmlDoc") (pointer "xmlNode") int)
            nonnull-char-string
            "xmlNodeListGetString"))

(define xml:ELEMENT-NODE
  ((c-lambda () int "___result = XML_ELEMENT_NODE;")))

(define (xml:attr-name attr)
  (xml:null-safe attr
    (c-lambda ((pointer "xmlAttribute"))
              nonnull-char-string
              "___result = ___arg1->name;")))

(define (xml:attr-children attr)
  (xml:null-safe attr
    (c-lambda ((pointer "xmlAttribute"))
              (pointer "xmlNode")
              "___result_voidstar = ___arg1->children;")))

(define (xml:attr-next attr)
  (xml:null-safe attr
    (c-lambda ((pointer "xmlAttribute"))
              (pointer "xmlAttribute")
              "___result_voidstar = ___arg1->next;")))

(define (xml:node->attr-alist node)
  (let loop ((result '())
             (attr (xml:node-properties node)))
    (if attr
        (loop (cons (cons (xml:attr-name attr)
                          (xml:node-list-string (xml:node-doc node)
                                                (xml:attr-children attr)
                                                1))
                    result)
              (xml:attr-next attr))
        result)))

(define (call-with-links head next-fn call-fn)
  (let loop ((head head))
    (if head
        (begin
          (call-fn head)
          (loop (next-fn head))))))

(define (xml:node->list node)
  (let ((result '()))
    (call-with-links node xml:node-next
      (lambda (node)
        (set! result (cons node result))))
    (reverse result)))

(define (xml:element? node)
  (= (xml:node-type node) xml:ELEMENT-NODE))

(define (xml:exp-make node-name node-attr-alist children)
  (cons node-name (cons node-attr-alist children)))

(define (xml:exp-name exp) (car exp))

(define (xml:exp-node-attr exp) (cadr exp))

(define (xml:exp-children exp) (cddr exp))

(define (xml:node->sexp-internal node)
  (map (lambda (node)
         (xml:exp-make (xml:node-name node)
                       (xml:node->attr-alist node)
                       (xml:node->sexp-internal (xml:node-children node))))
       (filter xml:element? (xml:node->list node))))

(define (xml:node->sexp node)
  (car (xml:node->sexp-internal node)))
