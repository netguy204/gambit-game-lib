module(..., package.seeall)

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

function vector_dist(a, b)
   local dx = a[1] - b[1]
   local dy = a[2] - b[2]
   return math.sqrt(dx * dx + dy * dy)
end

function thread(fn)
   if not fn then
      error('thread called with null function')
   end

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

function rand_between(lower, upper)
   local range = upper - lower
   return lower + math.random() * range
end

function table_copy(tbl)
   local new = {}
   for k, v in pairs(tbl) do
      new[k] = v
   end
   return new
end

function merge_into(target, source)
   if not source then
      return target
   end

   target = table_copy(target)
   for k, v in pairs(source) do
      target[k] = v
   end
   return target
end

NOT_RUNNING = 0
RUNNING = 1
DISABLING = 2

function switchable_part(opts)
   local defaults = {
      on_enable = function() end,
      on_disabling = function() end,
      on_disable = function() end,
      disable_time = 0,
      initial_state = NOT_RUNNING
   }
   local opts = merge_into(defaults, opts)

   local timer_message = nil
   if opts.disable_time > 0 then
      timer_message = constant.NEXT_EPHEMERAL_MESSAGE()
   end

   local state = opts.initial_state
   local first_run = true

   local behavior_part = function(go, comp)
      if first_run and opts.initial_state == RUNNING then
         first_run = false
         opts.on_enable(go, comp)
      end

      if go:has_message(constant.PLAYER_ACTION) then
         if state == NOT_RUNNING then
            state = RUNNING
            opts.on_enable(go, comp)
         elseif state == RUNNING then
            if opts.disable_time > 0 then
               state = DISABLING
               go:add_component("CTimer", {time_remaining=opts.disable_time,
                                           kind=timer_message})
               opts.on_disabling(go, comp)
            else
               state = NOT_RUNNING
               opts.on_disabling(go, comp)
               opts.on_disable(go, comp)
            end
         end
      elseif opts.disable_time > 0 and go:has_message(timer_message) then
         state = NOT_RUNNING
         opts.on_disable(go, comp)
      end
   end
   return behavior_part
end

function switchable_particle_system_part(system, initial_state)
   local disable_time = system.max_life or 0
   local initial_state = initial_state or NOT_RUNNING
   local component = nil

   local switch_on = function(go, comp)
      component = go:add_component("CParticleEmitter", system)
   end

   local switch_disable = function(go, comp)
      component:active(0)
   end

   local switch_off = function(go, comp)
      component:delete_me(1)
   end

   local switchability = util.switchable_part{on_enable=switch_on,
                                              on_disabling=switch_disable,
                                              on_disable=switch_off,
                                              disable_time=disable_time,
                                              initial_state=initial_state}
   return switchability
end

function thread_part(part)
   local loop = function(go, comp)
      while true do
         coroutine.yield()
         part(go, comp)
      end
   end

   return util.thread(loop)
end
