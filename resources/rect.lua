local M = {}

-- rect is minx, miny, mamxx, maxy
function M.union(a, b)
   return {
      math.min(a[1], b[1]),
      math.min(a[2], b[2]),
      math.max(a[3], b[3]),
      math.max(a[4], b[4])}
end

function M.center(a)
   return {(a[1] + a[3]) / 2,
           (a[2] + a[4]) / 2}
end

function M.width(a)
   return a[3] - a[1]
end

function M.height(a)
   return a[4] - a[2]
end

return M
