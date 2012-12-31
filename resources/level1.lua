-- world and player are globals

package.path = package.path .. ';resources/?.lua'

require 'human'
require 'constant'
require 'util'
require 'enemy'

function merge_into(target, source)
   if not source then
      return target
   end

   for k, v in pairs(source) do
      target[k] = v
   end
   return target
end

-- rect is minx, miny, mamxx, maxy
function rect_union(a, b)
   return {
      math.min(a[1], b[1]),
      math.min(a[2], b[2]),
      math.max(a[3], b[3]),
      math.max(a[4], b[4])}
end

function rect_center(a)
   return {(a[1] + a[3]) / 2,
           (a[2] + a[4]) / 2}
end

function rect_width(a)
   return a[3] - a[1]
end

function rect_height(a)
   return a[4] - a[2]
end

function stage_collidable(r)
   return stage:add_component("CCollidable", {w=rect_width(r),
                                              h=rect_height(r),
                                              offset=rect_center(r)})
end

function floor(minx, maxx, topy, art, opts)
   local midx = (minx + maxx) / 2
   local midy = topy - art.h / 2
   local w = maxx - minx

   local defaults = {entry=art, w=w,
                     layer=constant.BACKGROUND,
                     offset={midx, midy}}

   stage:add_component("CDrawHPatch", merge_into(defaults, opts))
   return {minx, topy - art.h, maxx, topy}
end

function wall(miny, maxy, midx, art, opts)
   local midy = (miny + maxy) / 2
   local h = maxy - miny

   local defaults = {entry=art, h=h,
                     layer=constant.BACKGROUND,
                     offset={midx, midy}}

   stage:add_component("CDrawVPatch", merge_into(defaults, opts))
   return {midx - art.w/2, miny, midx + art.w/2, maxy}
end

function round_to(val, nearest)
   return nearest * math.floor(val/nearest)
end

function dirt(minx, maxx, y)
   local _dirt = world:atlas_entry(constant.ATLAS, "dirt")
   return floor(round_to(minx, _dirt.w), round_to(maxx, _dirt.w), y, _dirt)
end

function grass(minx, maxx, y)
   local _grass = world:atlas_entry(constant.ATLAS, "grass")
   local base = dirt(minx, maxx, y)
   floor(round_to(minx, _grass.w), round_to(maxx, _grass.w),
         y + _grass.h/2, _grass,
         {layer=constant.FOREGROUND})

   return base
end


function door_behavior(go)
   CLOSED = 1
   OPEN = 2

   state = CLOSED

   local _door = world:atlas_entry(constant.ATLAS, "door")
   local _door_open = world:atlas_entry(constant.ATLAS, "door_open")
   local sprite = go:find_component("CStaticSprite")

   while true do
      coroutine.yield()

      local dist = util.vector_dist(go:pos(), player:pos())
      if state == CLOSED and dist < 100 then
         state = OPEN
         sprite:entry(_door_open)
      elseif state == OPEN and dist > 120 then
         state = CLOSED
         sprite:entry(_door)
      end
   end
end

function left_door(maxx, miny)
   local _door = world:atlas_entry(constant.ATLAS, "door")
   local go = world:create_go()
   go:pos{maxx - _door.w / 2, miny + _door.h / 2}

   go:add_component("CStaticSprite", {entry=_door})
   go:add_component("CScripted", {update_thread=util.thread(door_behavior)})
   return go
end

function level_init()
   local _wood = world:atlas_entry(constant.ATLAS, "wood1")
   local _wall = world:atlas_entry(constant.ATLAS, "outside_wall")
   local _pillar = world:atlas_entry(constant.ATLAS, "pillar")

   human.init{32, 100}

   bottom = grass(-64*20, 128, 0)
   bottom = rect_union(bottom, dirt(-64*20, 1000, -64))
   bottom = rect_union(bottom, dirt(-64*20, 1000, -64*2))
   bottom = rect_union(bottom, dirt(-64*20, 1000, -64*3))
   bottom = rect_union(bottom, dirt(-64*20, 1000, -64*4))
   bottom = rect_union(bottom, dirt(-64*20, 1000, -64*5))
   bottom = rect_union(bottom, floor(128, 1000, 0, _wood))

   stage_collidable(bottom)

   wall(0, 64*8, 128 + 32, _pillar)
   stage_collidable(wall(64*2, 64*8, 128 + (64-12), _wall))
   left_door(128 + 64, 0)
end
