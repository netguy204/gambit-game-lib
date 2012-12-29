module(..., package.seeall)

require 'util'

SPEED = 600
JUMP_SPEED = 1200
JUMP_HEIGHT = 300
GRAV_ACCEL = util.accel(JUMP_SPEED, JUMP_HEIGHT)
WIDTH = 28
HEIGHT = 64
THROW_SPEED = 1200

function input_thread(go, dt)
   local fire_pressed = false
   local facing = 1
   local left_art = world:atlas_entry(constant.ATLAS, "guy-left")
   local right_art = world:atlas_entry(constant.ATLAS, "guy")

   while true do
      coroutine.yield()

      local input = world:input_state()
      local parent = go:transform_parent()
      local _vel = go:_vel()

      if input.action2 then
         if not fire_pressed then
            fire_pressed = true
         end
      elseif fire_pressed then
         local pos = go:pos()
         fire_pressed = false
         local abs_pos = {0,0}
         util.vector_add(abs_pos, pos, {0, HEIGHT})

         local abs_vel = {facing * THROW_SPEED / 3, THROW_SPEED}
         if parent then
            par_vel = parent:vel()
            util.vector_add(abs_vel, abs_vel, par_vel)
         end

         bomb(abs_pos, abs_vel)
      end

      local vel = {0, 0}
      _vel[1] = input.leftright * SPEED
      if math.abs(input.leftright) > 0.01 then
         facing = util.sign(input.leftright)
         if facing < 0 then
            go:find_component("CStaticSprite"):entry(left_art)
         else
            go:find_component("CStaticSprite"):entry(right_art)
         end
      end

      if input.action1 and parent then
         _vel[2] = JUMP_SPEED
      end

      if not input.action1 and not parent then
         _vel[2] = math.min(_vel[2], 0)
      end

      go:_vel(_vel)
   end
end

function init()
   player:_pos{100, 100}
   player:_vel{0, 0}
   camera:_pos{100, 100}
   camera:_vel{0, 0}

   -- make player collidable
   local art = world:atlas_entry(constant.ATLAS, "guy")
   player:add_component("CCollidable", {w=WIDTH, h=HEIGHT})
   player:add_component("CPlatformer", {grav_accel=GRAV_ACCEL})
   player:add_component("CStaticSprite", {entry=art})

   -- link up the camera and input
   camera:add_component("CCameraFocus", {focus=player})
   player:add_component("CScripted", {thread=util.thread(input_thread)})
end
