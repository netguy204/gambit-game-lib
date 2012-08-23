(c-declare #<<c-declare-end

#include <libxml/tree.h>
#include <libxml/parser.h>

c-declare-end
)

(##include "common.scm")

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

(define xml:node-children
  (c-lambda ((pointer "xmlNode"))
            (pointer "xmlNode")
            "___result_voidstar = ___arg1->children;"))

(define xml:node-next
  (c-lambda ((pointer "xmlNode"))
            (pointer "xmlNode")
            "___result_voidstar = ___arg1->next;"))

(define xml:node-name
  (c-lambda ((pointer "xmlNode"))
            nonnull-char-string
            "___result = ___arg1->name;"))

(define xml:node-content
  (c-lambda ((pointer "xmlNode"))
            nonnull-char-string
            "___result = ___arg1->content;"))

(define xml:node-properties
  (c-lambda ((pointer "xmlNode"))
            (pointer "xmlAttribute")
            "___result_voidstar = ___arg1->properties;"))

(define xml:attr-name
  (c-lambda ((pointer "xmlAttribute"))
            nonnull-char-string
            "___result = ___arg1->name;"))

(define xml:attr-elem
  (c-lambda ((pointer "xmlAttribute"))
            nonnull-char-string
            "___result = ___arg1->elem;"))

(define xml:attr-next
  (c-lambda ((pointer "xmlAttribute"))
            (pointer "xmlAttribute")
            "___result_voidstar = ___arg1->nexth;"))

(define (call-with-links head next-fn call-fn)
  (let loop ((head head))
    (if head
        (begin
          (call-fn head)
          (loop (next-fn head))))))

(define xml->sexp (doc)
  (define ))
