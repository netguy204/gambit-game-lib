#ifndef GAME_UI_H
#define GAME_UI_H

#include "testlib.h"
#include "spriteatlas.h"

typedef enum {
  FONT_SMALL,
  FONT_MEDIUM,
  FONT_FIXED,
  FONT_MAX
} FontSize;

SpriteList spritelist_from_8patch(SpriteList list, SpriteAtlas atlas,
                                  Rect screen_rect);

SpriteList spritelist_from_string(SpriteList list, SpriteAtlas atlas, FontSize size,
                                  const char* string, int bl_x, int bl_y);

SpriteList spritelist_from_consoletext(SpriteList list, SpriteAtlas atlas, const char* string,
                                       int bl_x, int bl_y, int width);

void gameui_submit();

#endif
