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

function map_index(map, row, col)
   if row < 1 or row > map.height or col < 1 or col > map.width then
      return nil
   end
   return (row - 1) * map.width + col
end

function index_map(map, row, col)
   local idx = map_index(map, row, col)
   if not idx then
      return {}
   end

   local speci = map.tiles[idx]
   if speci == 0 then
      return {index=idx}
   else
      local new_table = util.table_copy(map.specs[speci])
      new_table.index = idx
      return new_table
   end
end

function install_map(map)
   stage:add_component("CDrawTilemap", {map=map,
                                        w=screen_width*1.5,h=screen_height*1.5})

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
   local jump_speed = 800

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
      local v = query_map(map, rect.bl(r))
      if v[kind] then
         return v
      end

      v = query_map(map, rect.br(r))
      if v[kind] then
         return v
      end

      v = query_map(map, rect.tl(r))
      if v[kind] then
         return v
      end

      v = query_map(map, rect.tr(r))
      if v[kind] then
         return v
      end
      return false
   end

   local function add_antigrav_force(go)
      local mass = go:mass() * go:gravity_scale()
      local grav = world:gravity()
      grav[1] = grav[1] * -mass
      grav[2] = grav[2] * -mass
      go:apply_force(grav)
   end

   local function controls(go, comp)
      local climbing = false
      local have_key = false

      while true do
         coroutine.yield()
         local input = world:input_state()
         local can_climb = is_touching_kind('climbable')
         local vel = go:vel()
         --print("can_climb", can_climb, "climbing", climbing)

         vel[1] = speed * input.leftright
         if can_climb then
            add_antigrav_force(go)
            vel[2] = speed * input.updown
         end

         if input.action1 and platformer:parent() then
            vel[2] = jump_speed
         end

         go:vel(vel)

         if input.action3 then
            reset_world()
         end

         if is_touching_kind('deadly') then
            print("A failure. Sad.")
            reset_world()
         end

         local collectable = is_touching_kind('key')
         if collectable then
            -- remove the key
            map.tiles[collectable.index] = 0
            stage:find_component('CDrawTilemap', nil):map(map)
            local _key = world:atlas_entry(constant.ATLAS, "key")
            go:add_component("CStaticSprite", {entry=_key,offset={0,64}})
            have_key = true
         end

         if have_key and is_touching_kind('lock') then
            print("You're such a winner.")
            reset_world()
         end
      end
   end

   return controls
end

function level_init()
   local _brick = world:atlas_entry(constant.ATLAS, "brick")
   local _tile1 = world:atlas_entry(constant.ATLAS, "tile1")
   local _spike = world:atlas_entry(constant.ATLAS, "spike")
   local _dirt = world:atlas_entry(constant.ATLAS, "dirt")
   local _ladder = world:atlas_entry(constant.ATLAS, "ladder")
   local _key = world:atlas_entry(constant.ATLAS, "key")
   local _lock = world:atlas_entry(constant.ATLAS, "lock")

   -- tilemap test
   local map = flip_map(
      {
         width=20,
         height=16,
         tile_width=64,
         tile_height=64,
         specs={{image=_tile1, solid=true},
                {image=_spike, deadly=true},
                {image=_ladder, climbable=true},
                {image=_key, key=true},
                {image=_lock, lock=true}},
         tiles={0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                4, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 5, 0, 0,
                1, 1, 3, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 3, 1, 1,
                0, 0, 3, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 3, 0, 0,
                0, 0, 3, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 3, 0, 0,
                0, 0, 3, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 3, 0, 0,
                1, 3, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 3, 1,
                0, 3, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 3, 0,
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
