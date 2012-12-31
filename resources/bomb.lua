module(..., package.seeall)

require 'human'
require 'constant'
require 'util'

THROW_SPEED = 1600
DIM = 45
MAX_HEIGHT = 400
GRAV_ACCEL = util.accel(THROW_SPEED, MAX_HEIGHT)
DELAY = 6
EXPLODE_START = 0.3
CHAIN_FACTOR = 1

IDLE = 1
EXPLODING = 2

function behavior_thread(go, component)
   local set_timer = function(delay)
      go:add_component("CTimer", {kind=constant.TIMER_EXPIRED, time_remaining=delay})
   end

   set_timer(DELAY - EXPLODE_START)

   local state = IDLE
   local platformer = go:find_component("CPlatformer")

   while true do
      coroutine.yield()

      -- sticky if landed
      local parent = platformer:parent()
      if parent then
         local vel = go:vel()
         vel[1] = 0
         go:vel(vel)
      end

      if go:has_message(constant.TIMER_EXPIRED) or go:has_message(constant.EXPLOSION_NEARBY) then
         if state == IDLE then
            -- set the next timer
            state = EXPLODING
            set_timer(EXPLODE_START)
            local system = {entry=world:atlas_entry(constant.ATLAS, "expl1"),
                            nmax=10,
                            start_scale=0.7,
                            max_life=EXPLODE_START,
                            grav_accel=-30}
            go:add_component("CParticleEmitter", system)
            go:find_component("CStaticSprite"):delete_me(1)
         elseif state == EXPLODING then
            go:delete_me(1)
            go:broadcast_message(DIM * CHAIN_FACTOR, constant.EXPLOSION_NEARBY)
            return
         end
      end
   end
end

function make(pos, vel)
   local go = world:create_go()
   local bomb_art = world:atlas_entry(constant.ATLAS, "bomb")
   local spark_art = world:atlas_entry(constant.ATLAS, "spark")

   go:add_component("CStaticSprite", {entry=bomb_art, layer=constant.PLAYER})
   go:add_component("CPlatformer", {w=DIM, h=DIM, friction=1})
   go:pos(pos)
   go:vel(vel)


   local system = {entry=spark_art,
                   offset={0.0,bomb_art.h/2},
                   max_offset=0,
                   nmax=10,
                   start_color=7000,
                   end_color=5000,
                   start_scale=0,
                   layer=constant.BACKDROP}

   go:add_component("CParticleEmitter", system)
   go:add_component("CScripted", {message_thread=util.thread(behavior_thread)})
   return go
end
