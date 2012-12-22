#include "xmltools.h"

#include <stdio.h>

xmlNode* xml_rootchildren(char *filename) {
  xmlDoc* doc = xmlParseFile(filename);
  xmlNode* root = xmlDocGetRootElement(doc);
  return root->children;
}

void xml_free(xmlNode* node) {
  xmlFreeDoc(node->doc);
}

char* node_attr(xmlNode* node, char* name, char* defvalue) {
  char* result = (char*)xmlGetProp(node, (const xmlChar*)name);
  if(result == NULL) {
    if(strcmp(defvalue, "error") == 0) {
      fprintf(stderr, "line %d: attribute %s is required\n", node->line, name);
      exit(1);
    } else {
      return defvalue;
    }
  }

  return result;
}

int streq_(const char* str1, const char* str2) {
  return strcmp(str1, str2) == 0;
}
