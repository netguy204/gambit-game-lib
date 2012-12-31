-- world and player are globals

package.path = package.path .. ';resources/?.lua'

require 'human'
require 'constant'
require 'util'
require 'enemy'
require 'rect'

function merge_into(target, source)
   if not source then
      return target
   end

   for k, v in pairs(source) do
      target[k] = v
   end
   return target
end

function stage_collidable(r)
   return stage:add_component("CCollidable", {w=rect.width(r),
                                              h=rect.height(r),
                                              offset=rect.center(r)})
end

function floor(minx, maxx, topy, art, opts)
   local midx = (minx + maxx) / 2
   local midy = topy - art.h / 2
   local w = maxx - minx

   local defaults = {entry=art, w=w,
                     layer=constant.BACKGROUND,
                     offset={midx, midy}}

   stage:add_component("CDrawHPatch", merge_into(defaults, opts))
   return {minx, topy - art.h, maxx, topy}
end

function wall(miny, maxy, midx, art, opts)
   local midy = (miny + maxy) / 2
   local h = maxy - miny

   local defaults = {entry=art, h=h,
                     layer=constant.BACKGROUND,
                     offset={midx, midy}}

   stage:add_component("CDrawVPatch", merge_into(defaults, opts))
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
   local sprite = go:find_component("CStaticSprite")

   while true do
      coroutine.yield()

      local dist = util.vector_dist(go:pos(), player:pos())
      if state == CLOSED and dist < 100 then
         state = OPEN
         sprite:entry(_door_open)
      elseif state == OPEN and dist > 120 then
         state = CLOSED
         sprite:entry(_door)
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

function pillar(miny, maxy, midx)
   local _pillar = world:atlas_entry(constant.ATLAS, "pillar")
   local _pillar_cap = world:atlas_entry(constant.ATLAS, "pillar-cap")

   maxy = round_to(maxy, _pillar.h)
   wall(miny, maxy, midx, _pillar)

   local capy = maxy + _pillar_cap.h / 2
   stage:add_component("CStaticSprite", {offset={midx, capy}, entry=_pillar_cap})
end

function add_emitter_emitter(emittor_go, system, max_active_dist)

   local emission_behavior = function(go, comp)
      state = 1
      go:add_component("CTimer", {kind=constant.TIMER_EXPIRED, time_remaining=1})

      -- die as soon as our timer goes off
      while true do
         coroutine.yield()
         if go:has_message(constant.TIMER_EXPIRED) then
            if state == 1 then
               -- start shutting down the particle system
               state = 2
               go:add_component("CTimer", {kind=constant.TIMER_EXPIRED, time_remaining=system.max_life})
               local ps = go:find_component("CParticleEmitter")
               ps:active(0)
            elseif state == 2 then
               go:delete_me(1)
               return
            end
         end
      end
   end

   local emittor_behavior = function(go, comp)
      local reset_timer = function()
         go:add_component("CTimer", {kind=constant.TIMER_EXPIRED, time_remaining=0.3})
      end

      reset_timer()

      while true do
         coroutine.yield()

         -- timer messages wake us up and we kick out a steam particle
         -- emitter, but only if the player is around to enjoy it
         local dist = util.vector_dist(go:pos(), player:pos())
         if dist < max_active_dist then
            local sp = world:create_go()
            sp:pos(go:pos())
            sp:gravity_scale(-2)
            sp:add_component("CPlatformer", {w=0.1, h=0.1, density=0.1})
            sp:vel{300+600 * random_gaussian(),
                   util.rand_between(-1000, 0)}
            sp:add_component("CParticleEmitter", system)
            sp:add_component("CScripted", {message_thread=util.thread(emission_behavior)})
            --sp:add_component("CStaticSprite", {entry=world:atlas_entry(constant.ATLAS, "bomb")})
         end
         reset_timer()
      end
   end

   emittor_go:add_component("CScripted", {message_thread=util.thread(emittor_behavior)})
end


function steam_pipe(miny, maxy, midx)
   local _pipe = world:atlas_entry(constant.ATLAS, "outside_wall")
   local _smoke = world:atlas_entry(constant.ATLAS, "smoke")
   local _water = world:atlas_entry(constant.ATLAS, "spark")

   local go = world:create_go()
   go:pos{midx, (miny + maxy) / 2}
   go:add_component("CDrawVPatch", {entry=_pipe,
                                    layer=constant.BACKGROUND,
                                    h=maxy-miny})
   local lower_third = -((maxy-miny)/3)
   go:add_component("CParticleEmitter", {entry=_water,
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
                                         nmax=50})
   local system = {entry=_smoke,
                   max_offset=32,
                   coloring=constant.BW,
                   start_scale=0.3,
                   end_scale=0.7,
                   start_color=1,
                   end_color=1,
                   start_alpha=0.5,
                   end_alpha=0,
                   nmax=40,
                   max_life=1,
                   max_angular_speed=2,
                   max_speed=50,
                   grav_accel=-10,
                   layer=constant.FOREGROUND,
                   offset={0, lower_third}}

   add_emitter_emitter(go, system, 500)
end


function level_init()
   local _wood = world:atlas_entry(constant.ATLAS, "wood1")
   local _wall = world:atlas_entry(constant.ATLAS, "outside_wall")

   human.init{32, 100}

   local bottom = grass(-64*20, 128, 0)
   bottom = rect.union(bottom, dirt(128, 64*20, 0, {layer=constant.BACKDROP}))
   bottom = rect.union(bottom, dirt(-64*20, 64*20, -64))
   bottom = rect.union(bottom, dirt(-64*20, 64*20, -64*2))
   bottom = rect.union(bottom, dirt(-64*20, 64*20, -64*3))
   bottom = rect.union(bottom, dirt(-64*20, 64*20, -64*4))
   bottom = rect.union(bottom, dirt(-64*20, 64*20, -64*5))
   bottom = rect.union(bottom, floor(128, 64*20, 0, _wood))
   stage_collidable(bottom)

   local room_height = 64*5
   pillar(0, room_height, 128 + 32)
   stage_collidable(wall(64*2, room_height, 128 + (64-12), _wall))
   stage_collidable(wall(0, room_height, 64*20 - _wall.w/2, _wall))
   stage_collidable(floor(192, 64*20, room_height, _wood))
   left_door(128 + 64, 0)

   steam_pipe(0, room_height, 64*10)
end
