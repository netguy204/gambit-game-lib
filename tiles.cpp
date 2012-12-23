#include "tiles.h"
#include "heapvector.h"
#include "utils.h"

#include <math.h>
#include <stdio.h>
#include <assert.h>

TileMap tilemap_make(int width, int height, int tw, int th) {
  int num_tiles = width * height;
  TileMap map = (TileMap)malloc(sizeof(struct TileMap_) + num_tiles);
  map->width_IT = width;
  map->height_IT = height;
  map->tile_width_IP = tw;
  map->tile_height_IP = th;
  map->x_bl = 0;
  map->y_bl = 0;
  return map;
}

void tilemap_free(TileMap map) {
  free(map);
}

int tilemap_index(TileMap map, TilePosition pos) {
  return map->width_IT * pos->y + pos->x;
}

void tileposition_tilemap(TilePosition pos, TileMap map, int index) {
  pos->x = index % map->width_IT;
  pos->y = index / map->width_IT;
}

int tilemap_index_vector(TileMap map, Vector vector) {
  float x = vector->x / map->tile_width_IP;
  float y = vector->y / map->tile_height_IP;

  return (int)floorf(x) + (int)floorf(y) * map->width_IT;
}

void vector_tilecenter(Vector v, TileMap map, int idx) {
  struct TilePosition_ pos;
  tileposition_tilemap(&pos, map, idx);

  v->x = map->tile_width_IP * pos.x + map->tile_width_IP / 2;
  v->y = map->tile_height_IP * pos.y + map->tile_height_IP / 2;
}

int tilemap_size(TileMap map) {
  return map->width_IT * map->height_IT;
}

int tilemap_validindex(TileMap map, TilePosition pos) {
  return pos->x >= 0 && pos->x < map->width_IT
    && pos->y >= 0 && pos->y < map->height_IT;
}


int tilemap_trace_line(TileMap map, TilePosition start, TilePosition end,
                        LineCallback callback, void* udata) {
  /* Bresenham's algorithm */
  struct TilePosition_ pos = *start;
  int dx = abs(start->x - end->x);
  int dy = abs(start->y - end->y);

  int sx, sy;
  if(start->x < end->x) {
    sx = 1;
  } else {
    sx = -1;
  }

  if(start->y < end->y) {
    sy = 1;
  } else {
    sy = -1;
  }

  int err = dx - dy;
  while(1) {
    int result = callback(map, &pos, udata);
    if(result) return result;

    if (pos.x == end->x && pos.y == end->y) return 0;
    int e2 = 2 * err;
    if (e2 > -dy) {
      err = err - dy;
      pos.x = pos.x + sx;
    }
    if (e2 < dx) {
      err = err + dx;
      pos.y =  pos.y + sy;
    }
  }
}


int clamp(int val, int min, int max) {
  if(val < min) return min;
  if(val > max) return max;
  return val;
}

SpriteList tilemap_spritelist(TileMap map, float x_bl, float y_bl, float wpx, float hpx) {
  float mx_bl = x_bl - map->x_bl;
  float my_bl = y_bl - map->y_bl;
  float mx_tr = mx_bl + wpx;
  float my_tr = my_bl + hpx;

  int tx_bl = clamp(floor(mx_bl / map->tile_width_IP), 0, map->width_IT);
  int ty_bl = clamp(floor(my_bl / map->tile_height_IP), 0, map->height_IT);
  int tx_tr = clamp(ceil(mx_tr / map->tile_width_IP), 0, map->width_IT);
  int ty_tr = clamp(ceil(my_tr / map->tile_height_IP), 0, map->height_IT);

  SpriteList spritelist = NULL;

  int ox = (int)floorf((map->x_bl + tx_bl * map->tile_width_IP) - x_bl);
  int oy = (int)floorf((map->y_bl + ty_bl * map->tile_height_IP) - y_bl);

  int xx, yy;
  for(yy = 0; yy < ty_tr - ty_bl; ++yy) {
    int yoffset = map->width_IT * (ty_bl + yy);
    int y = oy + (map->tile_height_IP * yy);

    for(xx = 0; xx < tx_tr - tx_bl; ++xx) {
      int offset = yoffset + tx_bl + xx;
      int x = ox + (map->tile_width_IP * xx);

      int tile = map->tiles[offset];
      TileSpec spec = &map->tile_specs[tile];
      if((spec->bitmask & TILESPEC_VISIBLE) == 0) continue;

      Sprite sprite = frame_make_sprite();
      sprite_fillfromentry(sprite, spec->image);
      sprite->originX = 0.0f;
      sprite->originY = 0.0f;
      sprite->displayX = (float)x;
      sprite->displayY = (float)y;
      spritelist = frame_spritelist_append(spritelist, sprite);
    }
  }

  return spritelist;
}

void tileposition_charimage(TilePosition pos, CharImage img, int index) {
  pos->x = index % img->w;
  pos->y = index / img->w;
}

int charimage_floodfill(CharImage out, CharImage input, TilePosition startpos,
                        int8_t value, FloodfillCallback callback, void* udata) {
  int8_t* memory = out->data;
  assert(charimage_size(input) == charimage_size(out));

  int max_index = charimage_size(input);
  int start = charimage_index(input, startpos->x, startpos->y);
  int kind = input->data[start];
  int row = input->w;
  HeapVector stack = heapvector_make();

  if(callback) callback(input, start, udata);
  memory[start] = value;
  HV_PUSH_VALUE(stack, int, start);
  int count = 0;

#define CAN_VISIT(pos) (((callback && callback(input, pos, udata)) || input->data[pos] == kind) \
                        && memory[pos] != value)

  while(stack->data_bytes > 0) {
    count += 1;

    int start = HV_POP_VALUE(stack, int);

    int above = start + row;
    if(above < max_index && CAN_VISIT(above)) {
      memory[above] = value;
      HV_PUSH_VALUE(stack, int, above);
    }

    int below = start - row;
    if(below >= 0 && CAN_VISIT(below)) {
      memory[below] = value;
      HV_PUSH_VALUE(stack, int, below);
    }

    int left = start - 1;
    if(left >= 0 && CAN_VISIT(left)) {
      memory[left] = value;
      HV_PUSH_VALUE(stack, int, left);
    }

    int right = start + 1;
    if(right < max_index && CAN_VISIT(right)) {
      memory[right] = value;
      HV_PUSH_VALUE(stack, int, right);
    }
  }

  heapvector_free(stack);
  return count;
}

void charimage_from_tilemap(CharImage img, TileMap map) {
  img->w = map->width_IT;
  img->h = map->height_IT;
  img->data = map->tiles;
}

void charimage_init_sizeof_tilemap(CharImage img, TileMap map) {
  img->w = map->width_IT;
  img->h = map->height_IT;
  img->data = (int8_t*)malloc(img->w * img->h);
}

void charimage_crosscorrelate(CharImage out, CharImage big, CharImage small) {
  int out_width = big->w - small->w;
  int out_height = big->h - small->h;

  assert(out->w >= out_width);
  assert(out->h >= out_height);

  int ox, oy, sx, sy;
  for(oy = 0; oy < out_height; ++oy) {
    for(ox = 0; ox < out_width; ++ox) {
      charimage_set(out, ox, oy, 0);

      for(sy = 0; sy < small->h; ++sy) {
        for(sx = 0; sx < small->w; ++sx) {
          int8_t prod = charimage_get(big, ox + sx, oy + sy)
            * charimage_get(small, sx, sy);
          out->data[charimage_index(out, ox, oy)] += prod;
        }
      }
    }
  }
}

int charimage_size(CharImage img) {
  return img->w * img->h;
}

void charimage_replace_value(CharImage img, int8_t from, int8_t to) {
  int xx;
  for(xx = 0; xx < charimage_size(img); ++xx) {
    if(img->data[xx] == from) {
      img->data[xx] = to;
    }
  }
}

void charimage_threshold(CharImage img, int8_t min) {
  int size = charimage_size(img);
  int ii;
  for(ii = 0; ii < size; ++ii) {
    if(img->data[ii] < min) {
      img->data[ii] = 0;
    }
  }
}

int label_floodfill_callback(CharImage img, int index, void* udata) {
  int8_t value = img->data[index];
  if(value == 0) return 0;

  HeapVector hv = (HeapVector)udata;

  struct LabelEntry_ entry;
  tileposition_charimage(&entry.pos, img, index);
  entry.value = value;
  HV_PUSH_VALUE(hv, struct LabelEntry_, entry);
  return 1;
}

void charimage_label(CharImage img, int8_t* working, LabelCallback callback, void* udata) {
  int size = charimage_size(img);
  memset(working, 0, size);

  HeapVector hv = heapvector_make();
  struct CharImage_ out = { img->w, img->h, working };

  int ii;
  for(ii = 0; ii < size; ++ii) {
    // if this is a new region
    if(img->data[ii] != 0 && working[ii] == 0) {
      heapvector_clear(hv);

      struct TilePosition_ pos;
      tileposition_charimage(&pos, img, ii);

      charimage_floodfill(&out, img, &pos, 1, label_floodfill_callback, hv);

      callback((LabelEntry)hv->data, hv->data_bytes / sizeof(struct LabelEntry_), udata);
    }
  }

  heapvector_free(hv);
}

void charimage_write(CharImage img, FILE* target) {
  int ii, jj;
  for(jj = 0; jj < img->h; ++jj) {
    for(ii = 0; ii < img->w; ++ii) {
      if(ii > 0) {
        fprintf(target, ", ");
      }
      fprintf(target, "%d", (int)charimage_get(img, ii, jj));
    }
    fprintf(target, "\n");
  }
}

void charimage_spit(CharImage img, const char* filename) {
  FILE* target = fopen(filename, "w");
  if(target == NULL) {
    fail_exit("charimage_spit: couldn't open %s", filename);
  }
  charimage_write(img, target);
  fclose(target);
}

static int region[][3] = {
  {0, 1, 2},
  {1, 1, 3},
  {1, 0, 2},
  {1, -1, 3},
  {0, -1, 2},
  {-1, -1, 3},
  {-1, 0, 2},
  {-1, 1, 3}
};

static int nregion = sizeof(region) / 3;

// chars are ~dist scaled by 2
char ambient_occlusion(TileMap map, int xx, int yy) {
  int zz;
  char result = 0;
  for(zz = 0; zz < nregion; ++zz) {
    int ox = region[zz][0];
    int oy = region[zz][1];
    int incr = region[zz][2];

    struct TilePosition_ tpos = {xx + ox, yy + oy};
    if(!tilemap_validindex(map, &tpos)) {
      // the world edges are occluders
      result += incr;
      continue;
    }

    int8_t kind = map->tiles[tilemap_index(map, &tpos)];
    if(map->tile_specs[kind].bitmask & TILESPEC_VISIBLE) {
      result += incr;
    }
  }
  return result;
}

void charimage_ambient_occlusion(CharImage occlusion, TileMap map) {
  int xx, yy;
  for(yy = 0; yy < map->height_IT; ++yy) {
    for(xx = 0; xx < map->width_IT; ++xx) {
      int ridx = charimage_index(occlusion, xx, yy);
      occlusion->data[ridx] = ambient_occlusion(map, xx, yy);
    }
  }
}