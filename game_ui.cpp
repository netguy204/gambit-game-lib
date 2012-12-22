#include "game_ui.h"
#include "heapvector.h"

#include "config.h"

#include <stdio.h>
#include <ctype.h>

typedef enum {
  EP_TL,
  EP_T,
  EP_TR,
  EP_L,
  EP_C,
  EP_R,
  EP_BL,
  EP_B,
  EP_BR,
  EP_MAX
} EightPatchElement;

const char* eight_patch_names[EP_MAX] = {
  "8patch_0.png",
  "8patch_1.png",
  "8patch_2.png",
  "8patch_3.png",
  "8patch_4.png",
  "8patch_5.png",
  "8patch_6.png",
  "8patch_7.png",
  "8patch_8.png"
};

SpriteAtlasEntry patch_cache[EP_MAX];
SpriteAtlasEntry font_cache[FONT_MAX][26];
SpriteAtlas atlas_cache = NULL;

void ensure_cache(SpriteAtlas atlas) {
  // 8 patch cache
  if(atlas_cache != atlas) {
    int ii;
    for(ii = 0; ii < EP_MAX; ++ii) {
      patch_cache[ii] = spriteatlas_find(atlas, eight_patch_names[ii]);
    }

    int jj;
    for(jj = 0; jj < FONT_MAX; ++jj) {
      for(ii = 0; ii < array_size(font_cache[FONT_SMALL]); ++ii) {
        char name_buffer[64];
        if(jj == FONT_SMALL) {
          snprintf(name_buffer, sizeof(name_buffer), "s_%d.png", ii + 1);
        } else if(jj == FONT_MEDIUM) {
          snprintf(name_buffer, sizeof(name_buffer), "%d.png", ii + 1);
        } else if(jj == FONT_FIXED) {
          snprintf(name_buffer, sizeof(name_buffer), "f_%d.png", ii + 1);
        }
        font_cache[jj][ii] = spriteatlas_find(atlas, name_buffer);
      }
    }

    atlas_cache = atlas;
  }
}

static Sprite ui_make_sprite(SpriteAtlasEntry entry, int x, int y) {
  Sprite sprite = frame_make_sprite();
  sprite_fillfromentry(sprite, entry);
  sprite->displayX = x;
  sprite->displayY = y;
  sprite->originX = 0;
  sprite->originY = 0;
  sprite->angle = 0;
  return sprite;
}

SpriteList spritelist_from_8patch(SpriteList list, SpriteAtlas atlas, Rect rect) {
  ensure_cache(atlas);

  int major_dim = patch_cache[EP_C]->w;
  int xsteps = (rect_width(rect) / major_dim) + 1;
  int ysteps = (rect_height(rect) / major_dim) + 1;
  int xpos = rect->minx;
  int ypos = rect->miny;

  int row, col;
  for(row = 0; row < ysteps; ++row) {
    int ymax = 0;
    for(col = 0; col < xsteps; ++col) {
      EightPatchElement element;

      if(row == 0) {
        if(col == 0) {
          element = EP_BL;
        } else if(col == (xsteps - 1)) {
          element = EP_BR;
        } else {
          element = EP_B;
        }
      } else if(row == (ysteps - 1)) {
        if(col == 0) {
          element = EP_TL;
        } else if(col == (xsteps - 1)) {
          element = EP_TR;
        } else {
          element = EP_T;
        }
      } else if(col == 0) {
        element = EP_L;
      } else if(col == (xsteps - 1)) {
        element = EP_R;
      } else {
        element = EP_C;
      }

      SpriteAtlasEntry entry = patch_cache[element];
      Sprite sprite = ui_make_sprite(entry, xpos, ypos);
      list = frame_spritelist_append(list, sprite);
      xpos += entry->w;
      ymax = MAX(ymax, entry->h);
    }

    ypos += ymax;
    xpos = rect->minx;
  }

  return list;
}

static int ui_ord(char ch) {
  int ord = toupper(ch) - 'A';

  // a better system would also try to resolve symbols
  if(ord < 0 || ord >= array_size(font_cache[FONT_SMALL])) {
    return -1;
  } else {
    return ord;
  }
}

int ui_string_width(SpriteAtlas atlas, FontSize size, const char* string) {
  ensure_cache(atlas);
  int width = 0;
  for(; *string; ++string) {
    int ord = ui_ord(*string);

    if(ord == -1) {
      // assume space
      width += font_cache[size][ui_ord('m')]->w;
    } else {
      width += font_cache[size][ord]->w;
    }
  }

  return width;
}

void ui_wrap_string(HeapVector output, const char* input, int max_chars) {
  const char* ip;
  int xpos;

  while(*input) {
    // count up next word
    for(ip=input;*ip && *ip != ' ';++ip);

    // insert newline if needed
    if(xpos + (ip - input) > max_chars) {
      char newline = '\n';
      HV_PUSH_VALUE(output, char, newline);
      xpos = 0;
    }

    // copy the word
    for(;input != ip; ++input) {
      HV_PUSH_VALUE(output, char, *input);
      ++xpos;
    }

    // copy any spaces
    while(*input == ' ') {
      HV_PUSH_VALUE(output, char, *input);
      ++input;
      ++xpos;
    }
  }
}


SpriteList spritelist_from_string(SpriteList list, SpriteAtlas atlas, FontSize size,
                                  const char* string, int bl_x, int bl_y) {
  ensure_cache(atlas);
  int xstart = bl_x;

  for(; *string; ++string) {
    if(*string == '\n') {
      bl_y -= font_cache[size][ui_ord('m')]->h;
      bl_x = xstart;
      continue;
    }

    int ord = ui_ord(*string);

    if(ord == -1) {
      // assume space
      bl_x += font_cache[size][ui_ord('m')]->w;
      continue;
    }

    SpriteAtlasEntry entry = font_cache[size][ord];
    Sprite sprite = ui_make_sprite(entry, bl_x, bl_y);
    list = frame_spritelist_append(list, sprite);
    bl_x += entry->w;
  }

  return list;
}

HeapVector consoletext_hv = NULL;

SpriteList spritelist_from_consoletext(SpriteList list, SpriteAtlas atlas, const char* string,
                                       int bl_x, int bl_y, int width) {
  ensure_cache(atlas);

  int char_width = font_cache[FONT_FIXED][ui_ord('m')]->w;
  int num_chars = width / char_width;

  if(consoletext_hv == NULL) {
    consoletext_hv = heapvector_make();
  }

  ui_wrap_string(consoletext_hv, string, num_chars);
  list = spritelist_from_string(list, atlas, FONT_FIXED, consoletext_hv->data, bl_x, bl_y);
  heapvector_clear(consoletext_hv);
  return list;
}

void gameui_submit() {

}
