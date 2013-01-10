local M = {}

M.ATLAS = "resources/images_default"

-- messages
M.COLLIDING = 0
M.TIMER_EXPIRED = 1
M.EXPLOSION_NEARBY = 2
M.PARENT_CHANGE = 3

-- script-only messages
M.PLAYER_ACTION = 100
M.PLAYER_JUMP = 101
M.PLAYER_JUMP_ABORT = 102

local function __make_ephemeral_counter()
   next_message = 1000
   local fn = function()
      local message = next_message
      next_message = next_message + 1
      return message
   end
   return fn
end

M.NEXT_EPHEMERAL_MESSAGE = __make_ephemeral_counter()

-- layers

M.BACKERDROP = 0
M.BACKDROP = 1
M.BACKGROUND = 2
M.PLAYER = 3
M.FOREGROUND = 4

-- colorings
M.BLACKBODY = 0
M.BW = 1

-- channels
M.EVENT = 0
M.FOLEY = 1
M.AMBIANCE = 2
M.STREAM = 3

-- body types
M.STATIC = 0
M.KINEMATIC = 1
M.DYNAMIC = 2

return M
