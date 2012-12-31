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

function background_floor(minx, maxx, topy, art, opts)
   local midx = (minx + maxx) / 2
   local midy = topy - art.h / 2
   local w = maxx - minx

   local defaults = {entry=art, w=w,
                     layer=constant.BACKGROUND,
                     offset={midx, midy}}

   c = stage:add_component("CDrawHPatch", merge_into(defaults, opts))
   return {c}
end

function background_wall(miny, maxy, midx, art, opts)
   local midy = (miny + maxy) / 2
   local h = maxy - miny

   local defaults = {entry=art, h=h,
                     layer=constant.BACKGROUND,
                     offset={midx, midy}}

   c = stage:add_component("CDrawVPatch", merge_into(defaults, opts))
   return {c}
end

function floor(minx, maxx, topy, art)
   local midx = (minx + maxx) / 2
   local midy = topy - art.h / 2
   local c = background_floor(minx, maxx, topy, art)
   local w = maxx - minx
   table.insert(c, stage:add_component("CCollidable", {w=w, h=art.h,
                                                       offset={midx, midy}}))
   return c
end

function wall(miny, maxy, midx, art)
   local midy = (miny + maxy) / 2
   local c = background_wall(miny, maxy, midx, art)
   local h = maxy - miny
   table.insert(c, stage:add_component("CCollidable", {w=art.w, h=h,
                                                       offset={midx, midy}}))
   return c
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
   local overlay = background_floor(round_to(minx, _grass.w),
                                    round_to(maxx, _grass.w),
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

   grass(-64*20, 128, 0)
   dirt(-64*20, 1000, -64)
   dirt(-64*20, 1000, -64*2)
   dirt(-64*20, 1000, -64*3)
   dirt(-64*20, 1000, -64*4)
   dirt(-64*20, 1000, -64*5)

   floor(128, 1000, 0, _wood)

   background_wall(0, 64*8, 128 + 32, _pillar)
   wall(64*2, 64*8, 128 + (64-12), _wall)
   left_door(128 + 64, 0)
end
