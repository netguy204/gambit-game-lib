-- world and player are globals

package.path = package.path .. ';resources/?.lua'

local human = require 'human'
local constant = require 'constant'
local util = require 'util'
local enemy = require 'enemy'
local rect = require 'rect'

_G.sounds = {explosion=world:get_sound("sounds/Explosion6.ogg", 0.3),
             action=world:get_sound("sounds/Jump20.ogg", 0.2),
             door=world:get_sound("sounds/door1.ogg", 0.2),
             music="sounds/DST-2ndBallad.ogg"}

function stage_collidable(r)
   return stage:add_component("CCollidable", {w=rect.width(r),
                                              h=rect.height(r),
                                              offset=rect.center(r)})
end

function floor(minx, maxx, topy, art, opts)
   local midx = (minx + maxx) / 2
   local midy = topy - art.h / 2
   local w = maxx - minx

   local defaults = {entry=art, w=w, h=art.h,
                     layer=constant.BACKGROUND,
                     offset={midx, midy}}

   stage:add_component("CDrawWallpaper", util.merge_into(defaults, opts))
   return {minx, topy - art.h, maxx, topy}
end

function wall(miny, maxy, midx, art, opts)
   local midy = (miny + maxy) / 2
   local h = maxy - miny

   local defaults = {entry=art, h=h, w=art.w,
                     layer=constant.BACKGROUND,
                     offset={midx, midy}}

   stage:add_component("CDrawWallpaper", util.merge_into(defaults, opts))
   return {midx - art.w/2, miny, midx + art.w/2, maxy}
end

function round_to(val, nearest)
   return nearest * math.floor(val/nearest)
end

function dirt(minx, maxx, y, opts)
   local _dirt = world:atlas_entry(constant.ATLAS, "dirt")
   return floor(round_to(minx, _dirt.w), round_to(maxx, _dirt.w), y, _dirt, opts)
end

function grass(minx, maxx, y)
   local _grass = world:atlas_entry(constant.ATLAS, "grass")
   local base = dirt(minx, maxx, y)
   floor(round_to(minx, _grass.w), round_to(maxx, _grass.w),
         y + _grass.h/2, _grass,
         {layer=constant.FOREGROUND})

   return base
end

function wallpaper(r, art, opts)
   local defaults = {w=rect.width(r),
                     h=rect.height(r),
                     offset=rect.center(r),
                     entry=art}

   stage:add_component("CDrawWallpaper", util.merge_into(defaults, opts))
   return r
end

-- flips the map so that the top row ends up on the top of the screen
-- like you would expect if you just visualized the map numbers
function flip_map(map)
   local new_tiles = {}
   local ntiles = map.width * map.height
   for idx=1,ntiles do
      row = math.floor((idx - 1) / map.width) + 1
      col = (idx - 1) % map.width + 1
      new_idx = (map.height - row) * map.width + col
      new_tiles[idx] = map.tiles[new_idx]
   end
   local new_map = util.table_copy(map)
   new_map.tiles = new_tiles
   return new_map
end

function level_init()
   local _brick = world:atlas_entry(constant.ATLAS, "brick")
   local _spike = world:atlas_entry(constant.ATLAS, "spike")
   local _dirt = world:atlas_entry(constant.ATLAS, "dirt")
   local _ladder = world:atlas_entry(constant.ATLAS, "ladder")

   -- player and floor
   human.init{-32, 100}

   local bottom = grass(-64*40, 64*80, 0)
   bottom = rect.union(bottom, wallpaper({-64*40, -64*10, 64*80, -64}, _dirt))
   stage_collidable(bottom)

   -- tilemap test
   local map = {
      width=20,
      height=8,
      tile_width=64,
      tile_height=64,
      specs={{image=_brick},
             {image=_spike},
             {image=_ladder}},
      tiles={0, 3, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 3, 0,
             0, 3, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 3, 0,
             0, 3, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 3, 0,
             1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1,
             0, 0, 3, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 3, 0, 0,
             0, 0, 3, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 3, 0, 0,
             1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1,
             1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2, 2, 1, 1, 1, 1, 1, 1, 1}}
   stage:add_component("CDrawTilemap", {map=flip_map(map),
                                        w=screen_width,h=screen_height})

end
