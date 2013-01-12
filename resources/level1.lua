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

local function add_bridgeness(go, steps)
   local extent = 1
   local speed_sps = 4
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
         wallpaper:w(_art.w * extent)
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
   local _current = world:atlas_entry(constant.ATLAS, "switch")
   local _other = world:atlas_entry(constant.ATLAS, "/xswitch")

   local sprite = go:add_component('CStaticSprite', {entry=_current})
   local message_thread = function(go, comp)
      while true do
         coroutine.yield()
         if go:has_message(ACTIVATE) then
            target:send_message(go:create_message(ACTIVATE_BRIDGE))

            -- swap the switch image
            sprite:entry(_other)
            local temp = _current
            _current = _other
            _other = temp
         end
      end
   end

   go:add_component('CScripted', {message_thread=util.thread(message_thread)})
   go:add_component('CSensor', {w=_current.w, h=_current.h})
end

local function add_playerness(player, m)
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

   local activate_trigger = util.rising_edge_trigger(false)
   local climbing = false
   local have_key = false
   local facing = 1

   local function controls(go, comp)
      while true do
         coroutine.yield()
         local input = world:input_state()
         local can_climb = is_touching_kind('climbable')
         local vel = vector.new(go:vel())

         if input.leftright < -0.01 then
            facing = -1
         elseif input.leftright > 0.01 then
            facing = 1
         end

         vel[1] = speed * input.leftright
         if can_climb then
            util.add_antigrav_force(go)
            vel[2] = speed * input.updown
         end

         local parent = platformer:parent()
         if input.action1 and parent then
            vel[2] = jump_speed
         end

         if activate_trigger(input.action2) then
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
            local _key = world:atlas_entry(constant.ATLAS, "key")
            go:add_component("CStaticSprite", {entry=_key,offset={0,64}})
            have_key = true
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
   local _key = world:atlas_entry(constant.ATLAS, "key")
   local _lock = world:atlas_entry(constant.ATLAS, "lock")
   local _platform = world:atlas_entry(constant.ATLAS, "platform")

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
         tiles={0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 6,
                0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 6,
                0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 6,
                1, 1, 3, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 6,
                0, 0, 3, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 6,
                0, 0, 3, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 6,
                0, 0, 3, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 6,
                1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 3, 1,
                0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 3, 0,
                0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 3, 0,
                0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 3, 0,
                1, 1, 3, 1, 1, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 3, 1, 1,
                0, 0, 3, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 3, 0, 0,
                5, 0, 3, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 3, 0, 0,
                1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1,
                1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2, 2, 1, 1, 1, 1, 1, 1, 1}
      })

   m:update_visuals()
   m:update_colliders()

   player:pos(m:center(3,17))
   add_playerness(player, m)

   -- add the key
   local key = world:create_go()
   key:pos(m:center(14, 1))
   key:add_component('CPlatformer', {w=_key.w,h=_key.h})
   key:add_component('CStaticSprite', {entry=_key})
   add_collectableness(key, _key.w, _key.h, player, COLLECTED)

   -- place the moving platform
   local start = vector.new(m:center(5, 6))
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
end
