#ifndef XMLTOOLS_H
#define XMLTOOLS_H

#include <libxml/tree.h>
#include <libxml/parser.h>

xmlNode* xml_rootchildren(char *filename);
void xml_free(xmlNode* node);

#endif
