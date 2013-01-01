module(..., package.seeall)

require 'util'
require 'bomb'

SPEED = 600
JUMP_SPEED = 1200
WIDTH = 28
HEIGHT = 62

function rising_edge_trigger(state)
   trigger = function(input)
      local result = false
      if input ~= state and input then
         result = true
      end
      state = input
      return result
   end
   return trigger
end

SELECTABLE_CATEGORY = 4

function is_selectable(go)
   local comp = go:find_component("CCollidable", nil)
   while comp do
      if comp:category() == SELECTABLE_CATEGORY then
         return comp
      end
      comp = go:find_component("CCollidable", comp)
   end
   return nil
end

function make_selectable(go, opts)
   local old_comp = is_selectable(go)
   if old_comp then
      old_comp:delete_me(1)
   end

   local defaults = {w=32, h=32, category=SELECTABLE_CATEGORY, mask=0}
   go:add_component("CCollidable", util.merge_into(defaults, opts))
end

function input_thread(go)
   local fire_pressed = false
   local interrupt_pressed = false

   local facing = 1
   local left_art = world:atlas_entry(constant.ATLAS, "guy-left")
   local right_art = world:atlas_entry(constant.ATLAS, "guy")
   local platformer = go:find_component("CPlatformer", nil)
   local state = FALLING
   local sprite = go:find_component("CStaticSprite", nil)

   local last_mark = nil
   local select_trigger = rising_edge_trigger(false)
   local select_triggered = false

   local set_mark = function(new_go)
      if last_mark then
         last_mark:find_component("CTestDisplay", nil):delete_me(1)
      end
      if new_go then
         new_go:add_component("CTestDisplay", {w=32,h=32,a=0.5})
      end
      last_mark = new_go
   end

   while true do
      coroutine.yield()

      local input = world:input_state()
      local dx = input.leftright
      local dy = input.updown
      local dead_zone = 0.5

      local have_direction = math.abs(dx) > 0.01 or math.abs(dy) > 0.01
      if math.abs(dx) > dead_zone then
         dx = util.sign(dx)
      end
      if math.abs(dy) > dead_zone then
         dy = util.sign(dy)
      end

      -- during selection, jump resets the mark to the player
      if fire_pressed and input.action1 then
         set_mark(go)
      end

      if (last_mark and not fire_pressed) then
         -- here's where we do something with our selection
         select_trigger(false)
         select_triggered = false
         if last_mark then
            last_mark:send_message(go:create_message(constant.PLAYER_ACTION))
            set_mark(nil)
         end
      end

      local angle = math.atan2(dy, dx)
      if fire_pressed then
         -- if nothing is selected, select the player
         if not last_mark then
            set_mark(go)
         end

         select_triggered = select_trigger(have_direction and angle)
         world:time_scale(0)
      else
         world:time_scale(1)
      end

      -- cycle the mark to the next object. right now we start the
      -- cone at the last mark but we should start the cone at the
      -- mark it was at for the current direction of travel and only
      -- reset when the direction changes
      if fire_pressed and select_triggered then
         local next_mark = world:next_in_cone(last_mark:pos(), last_mark, angle, 0.6)

         -- keep searching till we find something selectable
         while next_mark and not is_selectable(next_mark) do
            next_mark = world:next_in_cone(last_mark:pos(), next_mark, angle, 0.6)
         end

         -- if we found nothing, select the player who is always
         -- selectable
         if not next_mark then
            next_mark = go
         end

         set_mark(next_mark)
      end


      local parent = platformer:parent()
      local vel = go:vel()

      if input.action2 then
         if not fire_pressed then
            fire_pressed = true
         end
      elseif fire_pressed then
         fire_pressed = false

         local pos = go:pos()
         pos[2] = pos[2] + HEIGHT

         local vel = {facing * bomb.THROW_SPEED / 3, bomb.THROW_SPEED}
         make_selectable(bomb.make(pos, vel))
      end

      if math.abs(input.leftright) > 0.01 and not fire_pressed then
         vel[1] = input.leftright * SPEED
         facing = util.sign(input.leftright)
         if facing < 0 then
            sprite:entry(left_art)
         else
            sprite:entry(right_art)
         end
         go:vel(vel)
      elseif math.abs(vel[1]) > 0.01 then
         vel[1] = 0
         go:vel(vel)
      end

      if input.action1 and parent then
         vel[2] = JUMP_SPEED
         go:vel(vel)
      end

      if not input.action1 and not parent then
         vel[2] = math.min(vel[2], 0)
         go:vel(vel)
      end


      if input.action3 then
         if not interrupt_pressed then
            interrupt_pressed = true
         end
      elseif interrupt_pressed then
         interrupt_pressed = false
         util.printf('Memory: %d KB', collectgarbage("count"))
         reset_world()
      end
   end
end

function init(pos)
   player:pos(pos)
   player:vel{0, 0}
   make_selectable(player)

   camera:pos{100, 100}
   camera:vel{0, 0}

   -- make player collidable
   local art = world:atlas_entry(constant.ATLAS, "guy")
   player:add_component("CPlatformer", {w=WIDTH, h=HEIGHT, friction=0})
   player:add_component("CStaticSprite", {entry=art, layer=constant.PLAYER})
   --player:add_component("CTestDisplay", {w=WIDTH, h=HEIGHT, a=0.5, layer=constant.PLAYER})

   -- link up the camera and input
   world:focus(player)
   player:add_component("CScripted", {update_thread=util.thread(input_thread)})
end
