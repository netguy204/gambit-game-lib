-- world and player are globals

function accel(init_spd, max_height)
   return (init_spd * init_spd) / (2 * max_height)
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
enemy_gravity_accel = accel(1200, 300)

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

function fuzzy_enemy(go, dt)
   state = FALLING

   while true do
      coroutine.yield()
      if go:has_message(EXPLOSION_NEARBY) then
         go:send_terminate()
         return
      end

      if state == FALLING and go:transform_parent() then
         state = LANDED

         coll = go:find_component("CCollidable")
         go:_vel{enemy_speed, 0}

         w = coll:w()
         go:add_component("CLeftAndRight", {minx=-w/2, maxx=w/2})
      elseif state == LANDED and not go:transform_parent() then
         state = FALLING
         go:_vel{0, 0}
         go:find_component("CCollidable"):delete_me(1)
      end
   end

end

function player_coroutine(go, dt)
   timer = 5
   while true do
      timer = timer - dt
      if timer <= 0 then
         timer = 5
         print("hello from bubba", dt)
      end
      go, dt = coroutine.yield()
   end
end

function enemy(pos)
   local go = world:create_go()
   go:_pos(pos)

   local art = world:atlas_entry(ATLAS, "enemy")
   go:add_component("CStaticSprite", {entry=art})
   go:add_component("CCollidable", {w=enemy_dim, h=enemy_dim})
   go:add_component("CPlatformer", {grav_accel=enemy_gravity_accel})
   go:add_component("CScripted", {thread=coroutine.create(fuzzy_enemy)})
   return go
end

function level_init()
   player:_pos{100, 100}
   player:_vel{0, 0}

   enemy{200, 100}

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
