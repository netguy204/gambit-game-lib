#ifndef XMLTOOLS_H
#define XMLTOOLS_H

#include <libxml/tree.h>
#include <libxml/parser.h>
#include <string.h>

xmlNode* xml_rootchildren(char *filename);
void xml_free(xmlNode* node);
char* node_attr(xmlNode* node, char* name, char* defvalue);

int streq_(const char* str1, const char* str2);
#define streq(a, b) streq_((const char*)a, (const char*)b)

#endif
