-- world and player are globals

package.path = package.path .. ';resources/?.lua'

local human = require 'human'
local constant = require 'constant'
local util = require 'util'
local enemy = require 'enemy'
local rect = require 'rect'
local map = require 'map'

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

local function make_player_thread(m)
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
      return m:query_rect_corners(r, kind)
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
            m.tiles[collectable.index] = 0
            m:update_visuals()
            -- attach it to our head
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
   local _side_spike = world:atlas_entry(constant.ATLAS, "side_spike")
   local _dirt = world:atlas_entry(constant.ATLAS, "dirt")
   local _ladder = world:atlas_entry(constant.ATLAS, "ladder")
   local _key = world:atlas_entry(constant.ATLAS, "key")
   local _lock = world:atlas_entry(constant.ATLAS, "lock")

   -- tilemap test
   local m = map.create(
      {
         width=20,
         height=16,
         tile_width=64,
         tile_height=64,
         specs={{image=_tile1, solid=true},
                {image=_spike, deadly=true},
                {image=_ladder, climbable=true},
                {image=_key, key=true},
                {image=_lock, lock=true},
                {image=_side_spike, deadly=true}},
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

   m:update_visuals()
   m:update_colliders()

   player:add_component("CScripted", {update_thread=util.thread(make_player_thread(m))})
   player:pos{32, 170}

end
