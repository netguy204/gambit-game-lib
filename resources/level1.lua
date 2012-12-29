-- world and player are globals

function printf(...)
   print(string.format(...))
end

function accel(init_spd, max_height)
   return (init_spd * init_spd) / (2 * max_height)
end

function sign(val)
   if val > 0 then
      return 1
   else
      return -1
   end
end

function vector_add(out, a, b)
   out[1] = a[1] + b[1]
   out[2] = a[2] + b[2]
end

ATLAS = "resources/images_default"
TERMINATE = 2
TERMINATING = 3
COLLIDING = 4
TIMER_EXPIRED = 5
EXPLOSION_NEARBY = 6

FALLING = 1
LANDED = 2

enemy_speed = 100
enemy_dim = 36

player_width = 28
player_height = 64
player_speed = 600
player_jump_speed = 1200
player_jump_height = 300
throw_speed = 1200
grav_accel = accel(player_jump_speed, player_jump_height)

function plat(pos, w, h)
   local platform = world:create_go()
   platform:_pos(pos)

   local art = world:atlas_entry(ATLAS, "platform2")
   platform:add_component("CDrawPatch", {entry=art})
   platform:add_component("CCollidable", {w=w, h=h})
   return platform
end

function mplat(pos)
   local platform = plat(pos, 257, 64)
   platform:_vel{100, 0}

   platform:add_component("CLeftAndRight", {minx=128, maxx=1024})
   return platform
end

function thread(fn)
   local err = false
   local onerr = function(er)
      err = er
      print(debug.traceback(coroutine.running(), err, 2))
   end

   local threadfn = function(go, dt)
      local start = function()
         fn(go, dt)
      end

      xpcall(start, onerr)

      if err then
         -- an error on the following line is actually an err in fn,
         -- see the provided traceback for more detail.
         error(err)
      end
   end

   return coroutine.create(threadfn)
end

function player_input(go, dt)
   local fire_pressed = false
   local facing = 1
   local left_art = world:atlas_entry(ATLAS, "guy-left")
   local right_art = world:atlas_entry(ATLAS, "guy")

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
         vector_add(abs_pos, pos, {0, player_height})

         local abs_vel = {facing * throw_speed / 3, throw_speed}
         if parent then
            par_vel = parent:vel()
            vector_add(abs_vel, abs_vel, par_vel)
         end

         print "bang"
      end

      local vel = {0, 0}
      _vel[1] = input.leftright * player_speed
      if math.abs(input.leftright) > 0.01 then
         facing = sign(input.leftright)
         if facing < 0 then
            go:find_component("CStaticSprite"):entry(left_art)
         else
            go:find_component("CStaticSprite"):entry(right_art)
         end
      end

      if input.action1 and parent then
         _vel[2] = player_jump_speed
      end

      if not input.action1 and not parent then
         _vel[2] = math.min(_vel[2], 0)
      end

      go:_vel(_vel)
   end
end

function fuzzy_enemy(go, dt)
   local state = FALLING
   local old_vel = go:_vel()

   while true do
      coroutine.yield()
      if go:has_message(EXPLOSION_NEARBY) then
         go:send_terminate()
         return
      end

      local parented = go:transform_parent()
      if state == FALLING and parented then
         print('changed to LANDED')
         state = LANDED

         go:_vel{enemy_speed, 0}

         local w = parented:find_component("CCollidable"):w()
         printf("width is %d", w)
         go:add_component("CLeftAndRight", {minx=-w/2, maxx=w/2})
      elseif state == LANDED and not parented then
         print('changed to FALLING')
         state = FALLING
         go:_vel{0, 0}
         go:find_component("CLeftAndRight"):delete_me(1)
      elseif state == LANDED and go:has_message(COLLIDING) then
         -- bounce on collisions
         old_vel[1] = -old_vel[1]
         go:_vel(old_vel)
      end

      -- kill ourselves when we're falling out of the world
      if state == FALLING and go:_pos()[2] < -100 then
         go:send_terminate()
         return
      end

      old_vel = go:_vel()
   end

end

function enemy(pos)
   local go = world:create_go()
   go:_pos(pos)

   local art = world:atlas_entry(ATLAS, "enemy")
   go:add_component("CStaticSprite", {entry=art})
   go:add_component("CCollidable", {w=enemy_dim, h=enemy_dim})
   go:add_component("CPlatformer", {grav_accel=grav_accel})
   go:add_component("CScripted", {thread=thread(fuzzy_enemy)})
   return go
end

function player_init()
   player:_pos{100, 100}
   player:_vel{0, 0}
   camera:_pos{100, 100}
   camera:_vel{0, 0}

   -- make player collidable
   local art = world:atlas_entry(ATLAS, "guy")
   player:add_component("CCollidable", {w=player_width, h=player_height})
   player:add_component("CPlatformer", {grav_accel=grav_accel})
   player:add_component("CStaticSprite", {entry=art})

   -- link up the camera and input
   camera:add_component("CCameraFocus", {focus=player})
   player:add_component("CScripted", {thread=thread(player_input)})
end

function level_init()
   player_init()

   enemy{100, 300}

   plat({screen_width / 2, 32}, screen_width, 64)
   mplat{300, 300}
   mplat{600, 600}
   mplat{300, 900}
   mplat{600, 1200}
   mplat{300, 1500}
   mplat{600, 1800}
   mplat{300, 2100}
   mplat{600, 2400}
end
