module(..., package.seeall)

require 'human'
require 'constant'
require 'util'

THROW_SPEED = 1200
DIM = 48
MAX_HEIGHT = 400
GRAV_ACCEL = util.accel(THROW_SPEED, MAX_HEIGHT)
DELAY = 6
EXPLODE_START = 0.3
CHAIN_FACTOR = 2.0

IDLE = 1
EXPLODING = 2

function behavior_thread(go, component)
   local set_timer = function(delay)
      local message = go:create_message(constant.TIMER_EXPIRED)
      go:add_component("CTimer", {expire_message=message, time_remaining=delay})
   end

   set_timer(DELAY - EXPLODE_START)

   local state = IDLE
   while true do
      coroutine.yield()

      -- sticky if landed
      local parent = go:transform_parent()
      if parent then
         local vel = go:_vel()
         vel[1] = 0
         go:_vel(vel)
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
   go:_pos(pos)
   go:_vel(vel)

   local bomb_art = world:atlas_entry(constant.ATLAS, "bomb")
   local spark_art = world:atlas_entry(constant.ATLAS, "spark")

   go:add_component("CStaticSprite", {entry=bomb_art, layer=constant.PLAYER})
   go:add_component("CCollidable", {w=DIM, h=DIM})
   go:add_component("CPlatformer", {grav_accel=GRAV_ACCEL})

   local system = {entry=spark_art,
                   offset={0.0,16.0},
                   nmax=10,
                   grav_accel=80,
                   start_scale=0}

   go:add_component("CParticleEmitter", system)
   go:add_component("CScripted", {message_thread=util.thread(behavior_thread)})
   return go
end
