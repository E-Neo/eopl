local lfs = require "lfs"

local progress = function (path)
   local res = {0, 0, 0, 0, 0, 0, 0, 0, 0}
   for file in lfs.dir(path) do
      if string.find(file, "%d%d%d%.%a%a%a") then
         local chapter = tonumber(file:sub(1, 1))
         res[chapter] = res[chapter] + 1
      end
   end
   return res
end

local progress_bar = function (p)
   local n = math.floor(p / 0.025)
   local res = "["
   for _ = 1, n do res = res .. "#" end
   for _ = n, 40 do res = res .. " " end
   return res .. "]"
end

local main = function ()
   local total = {36, 31, 44, 42, 58, 40, 30, 27, 42}
   local prog = progress(arg[1])
   local s_total, s_prog = 0, 0
   for i = 1, 9 do
      local t, p = total[i], prog[i]
      s_total = s_total + t
      s_prog = s_prog + p
      print(string.format("%d: %s %3d/%d  (%.2f%%)",
                          i, progress_bar(p/t), p, t, p/t))
   end
   print(string.format("   %s %3d/%d (%.2f%%)",
                       progress_bar(s_prog/s_total), s_prog, s_total, s_prog/s_total))
end

main()
