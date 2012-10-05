#include "spriteatlas.h"

#include <libxml/tree.h>
#include <libxml/parser.h>
#include <string.h>

char* node_attr(xmlNode* node, char* name) {
  return (char*)xmlGetProp(node, (const xmlChar*)name);
}

void node_to_struct(xmlNode* node, ImageResource img, SpriteAtlasEntry entry) {
  char* entry_name = node_attr(node, "name");
  float x = atof(node_attr(node, "x"));
  float y = atof(node_attr(node, "y"));
  float width = atof(node_attr(node, "width"));
  float height = atof(node_attr(node, "height"));

  memset(entry, 0, sizeof(struct SpriteAtlasEntry_));
  strncpy(entry->name, entry_name, MAX_ENTRY_NAME);
  entry->u0 = x / image_width(img);
  entry->v0 = y / image_height(img);
  entry->u1 = (x + width) / image_width(img);
  entry->v1 = (y + height) / image_height(img);
  entry->w = width;
  entry->h = height;
}

int main(int argc, char ** argv) {
  if(argc != 4) {
    fprintf(stderr, "usage: %s [input] [input-img] [output]\n", argv[0]);
    return 1;
  }

  testlib_init();

  char * input_xml = argv[1];
  char * input_img = argv[2];
  char * output_name = argv[3];

  xmlDoc* doc = xmlParseFile(input_xml);
  xmlNode* root = xmlDocGetRootElement(doc);
  xmlNode* subtex = root->children;

  ImageResource img = image_load(input_img);
  FILE* output = fopen(output_name, "wb");

  int ii = 0;
  while(subtex) {
    if(subtex->type == XML_ELEMENT_NODE) {
      struct SpriteAtlasEntry_ entry;
      node_to_struct(subtex, img, &entry);
      fwrite(&entry, sizeof(struct SpriteAtlasEntry_), 1, output);
      ii++;
    }

    subtex = subtex->next;
  }

  fclose(output);
  xmlFreeDoc(doc);

  fprintf(stderr, "built %s\n", output_name);
  fprintf(stderr, "%d entries, %ld bytes per entry. %ld bytes total\n",
          ii, sizeof(struct SpriteAtlasEntry_),
          sizeof(struct SpriteAtlasEntry_) * ii);

  return 0;
}
