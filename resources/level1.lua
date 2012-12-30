-- world and player are globals

package.path = package.path .. ';resources/?.lua'

require 'human'
require 'constant'
require 'util'
require 'enemy'

function plat(pos, w, h)
   local platform = world:create_go()
   platform:_pos(pos)

   local art = world:atlas_entry(constant.ATLAS, "platform2")
   platform:add_component("CDrawHPatch", {entry=art})
   platform:add_component("CCollidable", {w=w, h=h})
   return platform
end

function mplat(pos)
   local platform = plat(pos, 257, 64)
   platform:_vel{100, 0}

   platform:add_component("CLeftAndRight", {minx=128, maxx=1024})
   return platform
end


function level_init()
   human.init()

   enemy.make{100, 300}

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
