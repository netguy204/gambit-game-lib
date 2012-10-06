#include "items.h"
#include "xmltools.h"

#include <string.h>

int main(int argc, char *argv[])
{
  if(argc != 2) {
    fprintf(stderr, "usage: %s [input]\n", argv[0]);
    return 1;
  }

  char * input_xml = argv[1];
  xmlNode* children = xml_rootchildren(input_xml);

  xml_free(children);

  return 0;
}
