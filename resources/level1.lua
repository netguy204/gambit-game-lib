-- world and player are globals

function plat(x, y, w, h)
   local platform = world:create_go()
   platform:pos(x, y)

   local art = world:atlas_entry("resources/images_default", "platform2")
   platform:add_component("CDrawPatch", {entry=art})
   local coll = platform:add_component("CCollidable", {w=w, h=h})
   return platform
end

function mplat(x, y)
   local platform = plat(x, y, 257, 64)
   platform:vel(100, 0)

   platform:add_component("CLeftAndRight", {minx=128, maxx=1024})
   return platform
end

function level_init()
   player:pos(100, 100)
   player:vel(0, 0)

   plat(screen_width / 2, 32, screen_width, 64)
   mplat(300, 300)
   mplat(600, 600)
   mplat(300, 900)
   mplat(600, 1200)
   mplat(300, 1500)
   mplat(600, 1800)
   mplat(300, 2100)
   mplat(600, 2400)
end
