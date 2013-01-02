module(..., package.seeall)

ATLAS = "resources/images_default"

-- messages
COLLIDING = 0
TIMER_EXPIRED = 1
EXPLOSION_NEARBY = 2
PARENT_CHANGE = 3

-- script-only messages
PLAYER_ACTION = 100
PLAYER_JUMP = 101
PLAYER_JUMP_ABORT = 102

function __make_ephemeral_counter()
   next_message = 1000
   local fn = function()
      local message = next_message
      next_message = next_message + 1
      return message
   end
   return fn
end

NEXT_EPHEMERAL_MESSAGE = __make_ephemeral_counter()

-- layers

BACKERDROP = 0
BACKDROP = 1
BACKGROUND = 2
PLAYER = 3
FOREGROUND = 4

-- colorings
BLACKBODY = 0
BW = 1

-- channels

EVENT = 0
FOLEY = 1
AMBIANCE = 2
STREAM = 3
