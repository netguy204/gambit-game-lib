module(..., package.seeall)

require 'constant'

FALLING = 1
LANDED = 2

SPEED = 100
DIM = 36


function behavior_thread(go, component)
   local state = FALLING
   local old_dir = 1

   while true do
      coroutine.yield()

      if go:has_message(constant.EXPLOSION_NEARBY) then
         print('explosion')
         go:delete_me(1)
         return
      end

      if state == LANDED and go:has_message(constant.COLLIDING) then
         -- bounce on collisions
         old_dir = -old_dir
         vel = go:_vel()
         vel[1] = old_dir * SPEED
         go:_vel(vel)
      end

      -- the change parent message ensures we'll get to check these
      -- conditions at appropriate times
      local parented = go:transform_parent()
      if state == FALLING and parented then
         print('changed to LANDED')
         state = LANDED

         go:_vel{SPEED, 0}

         local w = parented:find_component("CCollidable"):w()
         util.printf("width is %d", w)
         go:add_component("CLeftAndRight", {minx=-w/2, maxx=w/2})
      elseif state == LANDED and not parented then
         print('changed to FALLING')
         state = FALLING
         go:_vel{0, 0}
         go:find_component("CLeftAndRight"):delete_me(1)
      end

      -- kill ourselves when we're falling out of the world. FIXME:
      -- won't work message driven unless i add a timer
      if state == FALLING and go:_pos()[2] < -100 then
         go:delete_me(1)
         return
      end
   end

end

function make(pos)
   local go = world:create_go()
   go:_pos(pos)

   local art = world:atlas_entry(constant.ATLAS, "enemy")
   go:add_component("CStaticSprite", {entry=art})
   go:add_component("CCollidable", {w=DIM, h=DIM})
   go:add_component("CPlatformer", {grav_accel=human.GRAV_ACCEL})
   go:add_component("CScripted", {message_thread=util.thread(behavior_thread)})
   return go
end
