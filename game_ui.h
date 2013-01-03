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

BaseSprite spritelist_from_8patch(BaseSprite list, SpriteAtlas atlas,
                                  Rect screen_rect);

BaseSprite spritelist_from_string(BaseSprite list, SpriteAtlas atlas, FontSize size,
                                  const char* string, int bl_x, int bl_y);

BaseSprite spritelist_from_consoletext(BaseSprite list, SpriteAtlas atlas, const char* string,
                                       int bl_x, int bl_y, int width);

void gameui_submit();

#endif
