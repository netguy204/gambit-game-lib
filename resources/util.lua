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
