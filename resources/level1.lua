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


function door_behavior(go)
   CLOSED = 1
   OPEN = 2

   state = CLOSED

   local _door = world:atlas_entry(constant.ATLAS, "door")
   local _door_open = world:atlas_entry(constant.ATLAS, "door_open")
   local sprite = go:find_component("CStaticSprite", nil)

   while true do
      coroutine.yield()

      local dist = util.vector_dist(go:pos(), player:pos())
      if state == CLOSED and dist < 100 then
         state = OPEN
         sprite:entry(_door_open)
         world:play_sound(sounds.door, constant.EVENT)
      elseif state == OPEN and dist > 120 then
         state = CLOSED
         sprite:entry(_door)
         world:play_sound(sounds.door, constant.EVENT)
      end
   end
end

function left_door(maxx, miny)
   local _door = world:atlas_entry(constant.ATLAS, "door")
   local go = world:create_go()
   go:pos{maxx - _door.w / 2, miny + _door.h / 2}

   go:add_component("CStaticSprite", {entry=_door})
   go:add_component("CScripted", {update_thread=util.thread(door_behavior)})
   return go
end

function pillar(miny, maxy, midx, opt)
   local _pillar = world:atlas_entry(constant.ATLAS, "pillar")
   local _pillar_cap = world:atlas_entry(constant.ATLAS, "pillar-cap")

   maxy = round_to(maxy, _pillar.h)
   wall(miny, maxy, midx, _pillar, opt)

   local capy = maxy + _pillar_cap.h / 2
   local defaults = {offset={midx, capy},
                     entry=_pillar_cap}
   stage:add_component("CStaticSprite", util.merge_into(defaults, opt))
end

function steam_pipe(miny, maxy, midx)
   local _pipe = world:atlas_entry(constant.ATLAS, "outside_wall")
   local _steam = world:atlas_entry(constant.ATLAS, "smoke")
   local _water = world:atlas_entry(constant.ATLAS, "spark")
   local h = maxy - miny

   local go = world:create_go()
   human.make_selectable(go, {w=_pipe.w,h=h})

   go:pos{midx, (miny + maxy) / 2}
   go:add_component("CDrawWallpaper", {entry=_pipe,
                                       layer=constant.BACKGROUND,
                                       w=_pipe.w,
                                       h=h})

   local lower_third = -((maxy-miny)/3)
   local water_system = {entry=_water,
                         max_offset=5,
                         coloring=constant.BW,
                         start_color=1,
                         end_color=1,
                         start_alpha=0.7,
                         end_alpha=0.2,
                         max_life=1,
                         start_scale=0.1,
                         end_scale=0.4,
                         grav_accel=100,
                         offset={0, lower_third},
                         nmax=50}

   local steam_system = {entry=_steam,
                         max_offset=5,
                         coloring=constant.BW,
                         start_color=1,
                         end_color=1,
                         start_alpha=1,
                         end_alpha=0,
                         start_scale=0,
                         end_scale=1,
                         max_life=1,
                         max_angular_speed=1,
                         max_speed=5,
                         nmax=5,
                         grav_accel=-30,
                         offset={0, lower_third}}

   local thread = util.thread_part(util.switchable_particle_system_part(water_system, util.RUNNING))
   go:add_component("CScripted", {message_thread=thread})

   thread = util.thread_part(util.switchable_particle_system_part(steam_system, util.RUNNING))
   go:add_component("CScripted", {message_thread=thread})
end

function lawnmower(pos)
   local _smoke = world:atlas_entry(constant.ATLAS, "smoke")
   local particle_life = 1
   local system = {entry=_smoke,
                   max_offset=5,
                   coloring=constant.BW,
                   start_color=0,
                   end_color=0.4,
                   start_scale=0.1,
                   end_scale=0.4,
                   max_life=particle_life,
                   nmax=10,
                   start_alpha=1,
                   end_alpha=0,
                   grav_accel=-60}

   local behavior = function(go, comp)
      local state = NOT_RUNNING
      local switchability = util.switchable_particle_system_part(system)
      while true do
         coroutine.yield()
         switchability(go, comp)
         local expl_sender = go:has_message(constant.EXPLOSION_NEARBY)
         if expl_sender then
            -- want to move away from the sender
            local away = {0,0}
            util.vector_sub(away, go:pos(), expl_sender:pos())
            util.vector_norm(away, away)
            util.vector_scale(away, away, 600)
            local rand = {util.rand_between(-600, 600),
                          util.rand_between(0, 600)}
            util.vector_add(away, away, rand)
            go:vel(away)
         end
      end
   end

   local _art = world:atlas_entry(constant.ATLAS, "lawnmower")
   local go = world:create_go()
   go:pos(pos)

   go:add_component("CPlatformer", {w=_art.w, h=_art.h})
   go:add_component("CStaticSprite", {entry=_art})
   go:add_component("CScripted", {message_thread=util.thread(behavior)})
   human.make_selectable(go)

   return go
end

function wallpaper(r, art, opts)
   local defaults = {w=rect.width(r),
                     h=rect.height(r),
                     offset=rect.center(r),
                     entry=art}

   stage:add_component("CDrawWallpaper", util.merge_into(defaults, opts))
   return r
end

function level_init()
   local _wood = world:atlas_entry(constant.ATLAS, "wood1")
   local _wall = world:atlas_entry(constant.ATLAS, "outside_wall")
   local _dirt = world:atlas_entry(constant.ATLAS, "dirt")
   local _gold = world:atlas_entry(constant.ATLAS, "wallpaper-gold")
   local _aqua = world:atlas_entry(constant.ATLAS, "wallpaper-aqua")
   local _sky = world:atlas_entry(constant.ATLAS, "wallpaper-sky")

   human.init{32, 100}

   local room_height = 64*5
   pillar(0, room_height*2, 128 + 32, {layer=constant.BACKDROP})
   stage_collidable(wall(64*2, room_height*3, 128 + (64-12), _wall))
   stage_collidable(wall(0, room_height*3, 64*20 - _wall.w/2, _wall))

   -- first floor
   stage_collidable(floor(168, 64*16, room_height, _wood))
   wallpaper({64*3, 0, 64*20, room_height}, _gold)

   -- second and third
   stage_collidable(floor(192+64*4, 64*20, room_height*2, _wood))
   stage_collidable(floor(168, 64*20, room_height*3, _wood))
   wallpaper({64*3, room_height, 64*20, room_height*3}, _aqua)
   lawnmower({64*18, room_height*2.5})

   -- mini platforms
   stage_collidable(floor(64*18, 64*20, room_height*0.5, _wood))
   stage_collidable(floor(192, 192+64*2, room_height*1.5, _wood))


   left_door(128 + 64, 0)

   steam_pipe(room_height, room_height*2, 64*10)

   -- outdoors
   local bottom = grass(-64*40, 128, 0)
   bottom = rect.union(bottom, grass(64*20, 64*80,0))
   bottom = rect.union(bottom, dirt(128, 64*20, 0, {layer=constant.BACKDROP}))
   bottom = rect.union(bottom, wallpaper({-64*40, -64*10, 64*80, -64}, _dirt))
   bottom = rect.union(bottom, floor(128, 64*20, 0, _wood))
   stage_collidable(bottom)

   --wallpaper({-64*20, 0, 64*40, 64*40}, _sky, {layer=constant.BACKERDROP})

   -- play music twice
   --local next_sample = world:stream_sound(sounds.music, world:current_sound_sample())
   --world:stream_sound(sounds.music, next_sample)
end
