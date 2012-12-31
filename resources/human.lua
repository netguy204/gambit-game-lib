module(..., package.seeall)

require 'util'
require 'bomb'

SPEED = 600
JUMP_SPEED = 1200
WIDTH = 28
HEIGHT = 62

function input_thread(go)
   local fire_pressed = false
   local interrupt_pressed = false

   local facing = 1
   local left_art = world:atlas_entry(constant.ATLAS, "guy-left")
   local right_art = world:atlas_entry(constant.ATLAS, "guy")
   local platformer = go:find_component("CPlatformer")
   local state = FALLING
   local sprite = go:find_component("CStaticSprite")

   while true do
      coroutine.yield()

      local input = world:input_state()
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
         bomb.make(pos, vel)
      end

      if math.abs(input.leftright) > 0.01 then
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
   player:gravity_scale()

   camera:pos{100, 100}
   camera:vel{0, 0}

   -- make player collidable
   local art = world:atlas_entry(constant.ATLAS, "guy")
   player:add_component("CPlatformer", {w=WIDTH, h=HEIGHT, friction=0})
   player:add_component("CStaticSprite", {entry=art, layer=constant.PLAYER})

   -- link up the camera and input
   world:focus(player)
   player:add_component("CScripted", {update_thread=util.thread(input_thread)})
end
