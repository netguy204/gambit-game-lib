#include "xmltools.h"

xmlNode* xml_rootchildren(char *filename) {
  xmlDoc* doc = xmlParseFile(filename);
  xmlNode* root = xmlDocGetRootElement(doc);
  return root->children;
}

void xml_free(xmlNode* node) {
  xmlFreeDoc(node->doc);
}
