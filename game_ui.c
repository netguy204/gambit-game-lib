#include "game_ui.h"

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

char* eight_patch_names[EP_MAX] = {
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

SpriteList spritelist_from_string(SpriteList list, SpriteAtlas atlas, FontSize size,
                                  const char* string, int bl_x, int bl_y) {
  ensure_cache(atlas);

  for(; *string; ++string) {
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

void gameui_submit() {

}
