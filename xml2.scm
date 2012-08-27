(c-declare #<<c-declare-end

#include <libxml/tree.h>
#include <libxml/parser.h>

c-declare-end
)

(load "common")

(c-define-type xmlDoc (pointer "xmlDoc"))
(c-define-type xmlNode (pointer "xmlNode"))
(c-define-type xmlAttribute (pointer "xmlAttribute"))

(define xml:parse-file
  (c-lambda (nonnull-char-string)
            xmlDoc
            "xmlParseFile"))

(define xml:root-element
  (c-lambda (xmlDoc)
            xmlNode
            "xmlDocGetRootElement"))

(define xml:free-doc
  (c-lambda (xmlDoc)
            void
            "xmlFreeDoc"))

(define xml:node-children
  (c-lambda (xmlNode)
            xmlNode
            "___result_voidstar = ___arg1->children;"))

(define xml:node-next
  (c-lambda (xmlNode)
            xmlNode
            "___result_voidstar = ___arg1->next;"))

(define xml:node-name
  (c-lambda (xmlNode)
            nonnull-char-string
            "___result = ___arg1->name;"))

(define xml:node-content
  (c-lambda (xmlNode)
            nonnull-char-string
            "___result = ___arg1->content;"))

(define xml:node-properties
  (c-lambda (xmlNode)
            xmlAttribute
            "___result_voidstar = ___arg1->properties;"))

(define xml:node-type
  (c-lambda (xmlNode)
            int
            "___result = ___arg1->type;"))

(define xml:node-doc
  (c-lambda (xmlNode)
            xmlDoc
            "___result_voidstar = ___arg1->doc;"))

(define xml:node-list-string
  (c-lambda (xmlDoc xmlNode int)
            nonnull-char-string
            "xmlNodeListGetString"))

(define xml:ELEMENT-NODE
  ((c-lambda () int "___result = XML_ELEMENT_NODE;")))

(define xml:attr-name
  (c-lambda (xmlAttribute)
            nonnull-char-string
            "___result = ___arg1->name;"))

(define xml:attr-children
  (c-lambda (xmlAttribute)
            xmlNode
            "___result_voidstar = ___arg1->children;"))

(define xml:attr-next
  (c-lambda (xmlAttribute)
            xmlAttribute
            "___result_voidstar = ___arg1->next;"))

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

(define (sml:make node-name node-attr-alist children)
  (cons node-name (cons node-attr-alist children)))

(define (sml:name exp) (car exp))

(define (sml:attrs exp) (cadr exp))

(define (sml:attr exp attr)
  (let ((a (assoc attr (sml:attrs exp))))
    (if a (cdr a) #f)))

(define (sml:children exp) (cddr exp))

(define (xml->sml-internal node)
  (map (lambda (node)
         (sml:make (xml:node-name node)
                       (xml:node->attr-alist node)
                       (xml->sml-internal (xml:node-children node))))
       (filter xml:element? (xml:node->list node))))

(define (xml->sml node)
  (car (xml->sml-internal node)))

(define (sml:node-named? name)
  (lambda (node)
    (equal? (sml:name node) name)))

