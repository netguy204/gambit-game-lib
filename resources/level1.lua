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

function index_map(map, row, col)
   if row < 1 or row > map.height or col < 1 or col > map.width then
      return {}
   end

   local idx = (row - 1) * map.width + col
   local speci = map.tiles[idx]
   if speci == 0 then
      return {}
   else
      return map.specs[speci]
   end
end

function install_map(map)
   stage:add_component("CDrawTilemap", {map=map,
                                        w=screen_width,h=screen_height})

   local tile_rect = function(row, col)
      return { (col - 1) * map.tile_width,
               (row - 1) * map.tile_height,
               col * map.tile_width,
               row * map.tile_height }
   end

   local is_solid = function(row, col)
      return index_map(map, row, col).solid
   end

   -- build the colliders
   local current_collider = nil;
   for row=1,map.height do
      for col=1,map.width do
         if is_solid(row, col) then
            local tr = tile_rect(row, col)
            if current_collider then
               current_collider = rect.union(current_collider, tr)
            else
               current_collider = tr
            end
         elseif current_collider then
            util.stage_collidable(current_collider)
            current_collider = nil
         end
      end

      if current_collider then
         util.stage_collidable(current_collider)
         current_collider = nil
      end
   end

   return map
end

local function query_map(map, point)
   local col = math.floor(point[1] / map.tile_width) + 1
   local row = math.floor(point[2] / map.tile_height) + 1
   return index_map(map, row, col)
end

local function make_player_thread(map)
   local width = 28
   local height = 62
   local speed = 600
   local jump_speed = 1200

   local art = world:atlas_entry(constant.ATLAS, "guy")
   local platformer = player:add_component("CPlatformer", {w=width, h=height})
   player:add_component("CStaticSprite", {entry=art, layer=constant.PLAYER})

   local player_rect = function()
      local pos = player:pos()
      return { pos[1] - width / 2, pos[2] - height / 2,
               pos[1] + width / 2, pos[2] + height / 2}
   end

   local is_touching_kind = function(kind)
      local r = player_rect()
      return query_map(map, rect.bl(r))[kind] or
         query_map(map, rect.br(r))[kind] or
         query_map(map, rect.tl(r))[kind] or
         query_map(map, rect.tr(r))[kind]
   end

   local function controls(go, comp)
      local climbing = false

      while true do
         coroutine.yield()
         local input = world:input_state()
         local can_climb = is_touching_kind('climbable')
         --print("can_climb", can_climb, "climbing", climbing)

         if climbing and (not can_climb) then
            climbing = false
         elseif (not climbing) and can_climb then
            climbing = true
         end

         local vel = go:vel()
         vel[1] = speed * input.leftright
         if climbing then
            vel[2] = speed * input.updown
         elseif can_climb then
            vel[2] = 0
         end

         if input.action1 and platformer:parent() then
            vel[2] = jump_speed
         end

         --print("vel", vel[1], vel[2])
         go:vel(vel)

         if input.action3 then
            reset_world()
         end

         if is_touching_kind('deadly') then
            reset_world()
         end
      end
   end

   return controls
end

function level_init()
   local _brick = world:atlas_entry(constant.ATLAS, "brick")
   local _spike = world:atlas_entry(constant.ATLAS, "spike")
   local _dirt = world:atlas_entry(constant.ATLAS, "dirt")
   local _ladder = world:atlas_entry(constant.ATLAS, "ladder")

   local bottom = grass(-64*40, 64*80, 0)
   bottom = rect.union(bottom, wallpaper({-64*40, -64*10, 64*80, -64}, _dirt))
   util.stage_collidable(bottom)

   -- tilemap test
   local map = flip_map(
      {
         width=20,
         height=8,
         tile_width=64,
         tile_height=64,
         specs={{image=_brick, solid=true},
                {image=_spike, deadly=true},
                {image=_ladder, climbable=true},
                {image=_brick, climbable=true, solid=true}},
         tiles={0, 3, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 3, 0,
                0, 3, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 3, 0,
                0, 3, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 3, 0,
                1, 1, 3, 1, 1, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 3, 1, 1,
                0, 0, 3, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 3, 0, 0,
                0, 0, 3, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 3, 0, 0,
                1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1,
                1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2, 2, 1, 1, 1, 1, 1, 1, 1}
      })

   map = install_map(map)

   player:add_component("CScripted", {update_thread=util.thread(make_player_thread(map))})
   player:pos{32, 170}

end
