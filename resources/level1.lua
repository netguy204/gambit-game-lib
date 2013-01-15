-- world and player are globals

package.path = package.path .. ';resources/?.lua'

local human = require 'human'
local constant = require 'constant'
local util = require 'util'
local enemy = require 'enemy'
local rect = require 'rect'
local map = require 'map'
local vector = require 'vector'

_G.sounds = {explosion=world:get_sound("sounds/Explosion6.ogg", 0.3),
             action=world:get_sound("sounds/Jump20.ogg", 0.2),
             door=world:get_sound("sounds/door1.ogg", 0.2),
             music="sounds/DST-2ndBallad.ogg"}


local function add_platformness(platform, lower, upper, speed)
   local vel = (upper - lower):norm() * speed
   local dest = upper
   local other = lower

   -- we do all the position updates manually
   local thread = function(go, comp)
      go:body_type(constant.KINEMATIC)
      go:pos(lower)
      go:vel(vel)

      while true do
         coroutine.yield()

         local pos = go:pos()

         -- past our destination?
         if vel:dot(dest - pos) < 0 then
            -- bounce back the appropriate distance and reverse
            -- velocity
            local overshoot = (dest - pos)
            pos = dest + overshoot

            vel = vel * -1
            local temp = dest
            dest = other
            other = temp

            go:pos(pos)
            go:vel(vel)
         end
      end
   end

   thread = util.thread(thread)
   platform:add_component("CScripted", {update_thread=thread})
   platform:add_component("CCollidable", {w=64, h=16, mask=255})
end

-- sent to the player when they bump into something that's collectable
local COLLECTED = constant.NEXT_EPHEMERAL_MESSAGE()

-- message sent by player to touching things when action key is
-- pressed
local ACTIVATE = constant.NEXT_EPHEMERAL_MESSAGE()

-- message sent by switch to the bridge
local ACTIVATE_BRIDGE = constant.NEXT_EPHEMERAL_MESSAGE()

local function add_collectableness(go, w, h, collector, message)
   local message_thread = function(go, comp)
      while true do
         coroutine.yield()
         local msg = go:has_message(constant.COLLIDING)
         if msg and msg.source == collector then
            collector:send_message(go:create_message(message))
            go:delete_me(1)
         end
      end
   end

   go:add_component('CSensor', {w=w,h=h})
   go:add_component('CScripted', {message_thread=util.thread(message_thread)})
end

local function spawn_key(pos, vel)
   local _key = world:atlas_entry(constant.ATLAS, "key")
   local key = world:create_go()
   key:add_component('CPlatformer', {w=_key.w, h=_key.h, friction=100})
   key:add_component('CStaticSprite', {entry=_key})
   key:pos(pos)
   key:vel(vel)
   add_collectableness(key, _key.w, _key.h, player, COLLECTED)
end

local function add_bridgeness(go, steps)
   local extent = 1
   local speed_sps = 5
   local target = 1
   local dir = -1


   -- sync view and collisions with the current extent of the bridge
   local _art = world:atlas_entry(constant.ATLAS, 'blue_tile')
   local wallpaper = nil
   local collider = nil

   local update_components = function()
      local w = math.floor(extent) * _art.w
      local offset_x = w / 2 - _art.w / 2
      local offset = {offset_x, 0}
      if not wallpaper then
         wallpaper = go:add_component('CDrawWallpaper', {entry=_art, w=w, h=_art.h,
                                                         offset=offset})
      else
         wallpaper:w(w)
         wallpaper:offset(offset)
      end

      if collider then
         collider:delete_me(1)
      end
      collider = go:add_component('CCollidable', {w=w, h=_art.h, offset=offset})
   end

   update_components()

   local expand_thread_running = false

   local expand_thread = function(go, comp)
      expand_thread_running = true
      while true do
         coroutine.yield()
         local dt = world:dt()
         local old_extent = extent
         extent = extent + speed_sps * dt * dir
         if math.floor(extent) ~= math.floor(old_extent) then
            update_components()
         end

         -- if we reach our target then stop the update thread
         if math.floor(extent) == target then
            extent = target
            expand_thread_running = false
            update_components()
            return
         end
      end
   end

   local message_thread = function(go, comp)
      while true do
         coroutine.yield()
         if go:has_message(ACTIVATE_BRIDGE) then
            -- flip the target and activate the thread if necessary
            if target == 1 then
               target = steps
            else
               target = 1
            end
            dir = dir * -1

            if not expand_thread_running then
               comp:update_thread(util.thread(expand_thread))
            end
         end
      end
   end

   local thread = util.thread(message_thread)
   return go:add_component("CScripted", {message_thread=thread})
end


local function add_switchness(go, target)
   local _anim = world:animation("resources/lever.cs", constant.ATLAS, "First Animation")
   local next_dir = 1
   local sprite = go:add_component('CSpriterSprite', {animation=_anim, time_scale=0,
                                                      offset={0,-32}})
   local anim_running = false
   local switch_complete = constant.NEXT_EPHEMERAL_MESSAGE()

   local message_thread = function(go, comp)
      while true do
         coroutine.yield()
         if not anim_running and go:has_message(ACTIVATE) then
            sprite:time_scale(next_dir)
            anim_running = true
            next_dir = -next_dir

            -- register for the completion message
            go:add_component('CTimer', {time_remaining=_anim.length, kind=switch_complete})
         elseif anim_running and go:has_message(switch_complete) then
            anim_running = false
            sprite:time_scale(0)
            if next_dir == 1 then
               sprite:current_time(0)
            else
               sprite:current_time(_anim.length)
            end
            target:send_message(go:create_message(ACTIVATE_BRIDGE))
         end
      end
   end

   go:add_component('CScripted', {message_thread=util.thread(message_thread)})
   go:add_component('CSensor', {w=32, h=32})
end

local function add_springness(go, velocity)
   local _art = world:atlas_entry(constant.ATLAS, "spring")
   local sprite = go:add_component('CStaticSprite', {entry=_art})
   local messages = function(go, comp)
      while true do
         coroutine.yield()
         local msg = go:has_message(constant.COLLIDING)
         if msg then
            local obj = msg.source
            local vel = obj:vel()
            vel[2] = velocity
            obj:vel(vel)
         end
      end
   end
   go:add_component('CSensor', {w=_art.w, h=_art.h})
   go:add_component('CScripted', {message_thread=util.thread(messages)})
end

local function add_playerness(player, m)
   local width = 28
   local height = 62
   local speed = 600
   local jump_speed = 800

   local art = world:atlas_entry(constant.ATLAS, "guy")
   local _key = world:atlas_entry(constant.ATLAS, "key")
   local platformer = player:add_component("CPlatformer", {w=width, h=height, friction=0})
   --player:add_component("CStaticSprite", {entry=art, layer=constant.PLAYER})
   local _anim = world:animation('resources/mal.cs', constant.ATLAS, 'First Animation')
   local anim = player:add_component('CSpriterSprite', {animation=_anim, offset={0,-32}})

   local player_rect = function()
      local pos = player:pos()
      return { pos[1] - width / 2, pos[2] - height / 2,
               pos[1] + width / 2, pos[2] + height / 2}
   end

   local is_touching_kind = function(kind)
      local r = player_rect()
      return m:query_rect_corners(r, kind)
   end

   local activate_trigger = util.rising_edge_trigger(false)
   local jump_trigger = util.rising_edge_trigger(false)

   local climbing = false
   local have_key = nil
   local facing = 1

   local function controls(go, comp)
      while true do
         coroutine.yield()
         local input = world:input_state()
         local can_climb = is_touching_kind('climbable')
         local vel = vector.new(go:vel())
         local moving = false

         if input.leftright < -0.01 then
            facing = -1
            moving = true
         elseif input.leftright > 0.01 then
            facing = 1
            moving = true
         end

         if moving then
            anim:time_scale(1)
            anim:scale_x(facing)
         else
            anim:time_scale(0)
         end

         vel[1] = speed * input.leftright
         if can_climb then
            util.add_antigrav_force(go)
            if (not have_key) then
               vel[2] = speed * input.updown
            else
               vel[2] = 0
            end
         end

         local parent = platformer:parent()
         if jump_trigger(input.action1) then
            if have_key then
               -- throw
               local pos = vector.new(go:pos())
               pos = pos + {0, _key.h + 1}
               local vel = {jump_speed * facing * 0.5, jump_speed * 1.5}
               spawn_key(pos, vel)
               have_key:delete_me(1)
               have_key = nil
            elseif parent then
               vel[2] = jump_speed
            end
         end

         if (not have_key) and activate_trigger(input.action2) then
            go:broadcast_message(width, ACTIVATE)
         end

         if parent then
            local pvel = vector.new(parent:vel())
            if pvel:length() > 0 then
               vel = vel + pvel
            end
         end

         go:vel(vel)

         if input.action3 then
            reset_world()
         end

         if is_touching_kind('deadly') then
            print("A failure. Sad.")
            reset_world()
         end

         if have_key and is_touching_kind('lock') then
            print("You're such a winner.")
            reset_world()
         end
      end
   end

   local messages = function(go, comp)
      while true do
         coroutine.yield()
         if go:has_message(COLLECTED) then
            -- attach it to our head
            have_key = go:add_component("CStaticSprite", {entry=_key,offset={0,102}})
         end
      end
   end

   player:add_component('CScripted', {update_thread=util.thread(controls),
                                      message_thread=util.thread(messages)})
   return controls
end

function level_init()
   local _brick = world:atlas_entry(constant.ATLAS, "brick")
   local _tile1 = world:atlas_entry(constant.ATLAS, "tile1")
   local _spike = world:atlas_entry(constant.ATLAS, "spike")
   local _side_spike = world:atlas_entry(constant.ATLAS, "side_spike")
   local _dirt = world:atlas_entry(constant.ATLAS, "dirt")
   local _ladder = world:atlas_entry(constant.ATLAS, "ladder")
   local _lock = world:atlas_entry(constant.ATLAS, "lock")
   local _platform = world:atlas_entry(constant.ATLAS, "platform")
   local _platform_support = world:atlas_entry(constant.ATLAS, "platform-support")
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
                {image=_lock, lock=true},
                {image=_side_spike, deadly=true},
                {image=_platform_support}},
         tiles={0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 5,
                0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 5,
                0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 5,
                1, 1, 3, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 5,
                0, 0, 3, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 5,
                0, 0, 3, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 5,
                0, 0, 3, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 5,
                1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 3, 1,
                0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 3, 0,
                0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 3, 0,
                0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 3, 0,
                1, 1, 3, 1, 1, 1, 6, 6, 6, 6, 6, 6, 1, 1, 1, 1, 1, 3, 1, 1,
                0, 0, 3, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 3, 0, 0,
                4, 0, 3, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 3, 0, 0,
                1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1,
                1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2, 2, 1, 1, 1, 1, 1, 1, 1}
      })

   m:update_visuals()
   m:update_colliders()

   player:pos(m:center(3,17))
   add_playerness(player, m)

   -- Add the key
   spawn_key(m:center(14, 1), {0, 0})

   -- place the moving platform
   local start = vector.new(m:center(5, 7))
   local stop = vector.new(m:center(5, 12))
   local platform = world:create_go()
   platform:add_component("CStaticSprite", {entry=_platform})
   add_platformness(platform, start, stop, 100)

   -- place the bridge and switch
   local bridge = world:create_go()
   bridge:pos(m:center(9,5))
   add_bridgeness(bridge, 10)

   local switch = world:create_go()
   switch:pos(m:center(10, 15))
   add_switchness(switch, bridge)

   local spring = world:create_go()
   spring:pos(m:center(6,6))
   add_springness(spring, 1200)

   -- add the background
   local _background = world:atlas_entry('resources/sunrise_grad', 'sunrise_grad')
   stage:add_component('CStaticSprite', {entry=_background,
                                         offset={_background.w/2 - 32,
                                                 _background.h/2 - 32},
                                         layer=constant.BACKDROP})
end
