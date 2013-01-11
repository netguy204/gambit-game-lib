local M = {}

function M.new(v)
   setmetatable(v, M)
   return v
end

M.__index = M

function M.__add(a, b)
   return M.new{ a[1] + b[1], a[2] + b[2] }
end

function M.__sub(a, b)
   return M.new{ a[1] - b[1], a[2] - b[2] }
end

function M.norm(a)
   local len = a:length()
   return M.new{ a[1] / len, a[2] / len }
end

function M.length(v)
   return math.sqrt(v[1] * v[1] + v[2] * v[2])
end

function M.dist(a, b)
   local dx = a[1] - b[1]
   local dy = a[2] - b[2]
   return math.sqrt(dx * dx + dy * dy)
end

function M.__mul(a, s)
   return M.new{ a[1] * s, a[2] * s }
end

function M.dot(a, b)
   return a[1] * b[1] + a[2] * b[2]
end

return M
