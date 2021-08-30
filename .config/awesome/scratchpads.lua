local bling = require("lib.bling")
local rubato = require("lib.rubato")
local anim_y = rubato.timed {
    pos = 1090,
    rate = 60,
    easing = rubato.quadratic,
    intro = 0.1,
    duration = 0.3,
    awestore_compat = true
}

local anim_x = rubato.timed {
    pos = -970,
    rate = 60,
    easing = rubato.quadratic,
    intro = 0.1,
    duration = 0.3,
    awestore_compat = true 
}

local music_scratch = bling.module.scratchpad {
    command = "xterm -e ncmpcpp",
    rule = { instance = "music" },                 
    sticky = false,                                
    autoclose = true,                              
    floating = false,                              
    geometry = {x = 0 , y = 0},
    reapply = true,            
    dont_focus_before_close  = true,
    rubato = {x = anim_x, y = anim_y} 
}
awesome.connect_signal("scratch::music", function() music_scratch:toggle() end)

