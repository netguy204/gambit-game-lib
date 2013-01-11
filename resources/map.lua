local M = {}
local util = require 'util'
local rect = require 'rect'

-- flips the map so that the top row ends up on the top of the screen
-- like you would expect if you just visualized the map numbers
local function flip_map(map)
   local new_tiles = {}
   local ntiles = map.width * map.height
   for idx=1,ntiles do
      row = math.floor((idx - 1) / map.width) + 1
      col = (idx - 1) % map.width + 1
      new_idx = (map.height - row) * map.width + col
      new_tiles[idx] = map.tiles[new_idx]
   end
   local new_map = util.table_copy(map)
   new_map.tiles = new_tiles
   return new_map
end

function M.create(map)
   map = flip_map(map)
   map.__index = M
   setmetatable(map, map)
   return map
end

function M.index(map, row, col)
   if row < 1 or row > map.height or col < 1 or col > map.width then
      return nil
   end
   return (row - 1) * map.width + col
end

function M.fetch(map, row, col)
   local idx = map:index(row, col)
   if not idx then
      return {}
   end

   local speci = map.tiles[idx]
   if speci == 0 then
      return {index=idx}
   else
      local new_table = util.table_copy(map.specs[speci])
      new_table.index = idx
      return new_table
   end
end

function M.query(map, point)
   local col = math.floor(point[1] / map.tile_width) + 1
   local row = math.floor(point[2] / map.tile_height) + 1
   return map:fetch(row, col)
end

function M.query_rect_corners(map, r, kind)
   local v = map:query(rect.bl(r))
   if v[kind] then
      return v
   end

   v = map:query(rect.br(r))
   if v[kind] then
      return v
   end

   v = map:query(rect.tl(r))
   if v[kind] then
      return v
   end

   v = map:query(rect.tr(r))
   if v[kind] then
      return v
   end
   return false
end

-- install the map into the stage
function M.update_visuals(map)
   if map.draw_component then
      map.draw_component:map(map)
   else
      -- 1.5 is a magic special number that accounts for the scaling
      -- applied to our orthogonal transform in C
      local params = {map=map, w=screen_width*1.5,h=screen_height*1.5}
      map.draw_component = stage:add_component("CDrawTilemap", params)
   end
end

function M.update_colliders(map)
   -- if we already had colliders, remove them
   if map.colliders then
      for k, v in pairs(map.colliders) do
         v:delete_me(1)
      end
   end

   -- trying to build a fairly minimal set of rectangular colliders to
   -- account for the collidable tiles we have. If the collision
   -- portion of the map is ever subtracted from then you should
   -- probably rebuild the colliders
   local tile_rect = function(row, col)
      return { (col - 1) * map.tile_width,
               (row - 1) * map.tile_height,
               col * map.tile_width,
               row * map.tile_height }
   end

   local is_solid = function(row, col)
      return map:fetch(row, col).solid
   end

   -- build the colliders
   local current_collider = nil;
   local colliders = {}

   local finish_collider = function()
      local c = util.stage_collidable(current_collider)
      table.insert(colliders, c)
      current_collider = nil
   end

   for row=1,map.height do
      for col=1,map.width do
         if is_solid(row, col) then
            local tr = tile_rect(row, col)
            if current_collider then
               current_collider = rect.union(current_collider, tr)
            else
               current_collider = tr
            end
         elseif current_collider then
            finish_collider()
         end
      end

      if current_collider then
         finish_collider()
      end
   end

   -- remember our colliders in case we need to remove them
   map.colliders = colliders
   return map
end

return M
