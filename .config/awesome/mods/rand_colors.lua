-- Return Random Colors
local colors   = require("beautiful.xresources").get_current_theme()
local math = require('math')
function rand_colors()
   local i = math.random(1,15)
   -- if i == 1 then
   --    return tostring()
   -- elseif i ==  then

   -- end
   return tostring(i)
end
return rand_colors
