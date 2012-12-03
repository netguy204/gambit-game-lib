#ifndef GAME_UI_H
#define GAME_UI_H

#include "testlib.h"
#include "spriteatlas.h"

SpriteList spritelist_from_8patch(SpriteList list, SpriteAtlas atlas,
                                  Rect screen_rect);

SpriteList spritelist_from_string(SpriteList list, SpriteAtlas atlas,
                                  const char* string, int bl_x, int bl_y);

void gameui_submit();

#endif
