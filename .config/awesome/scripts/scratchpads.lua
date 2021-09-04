local bling = require("lib.bling")
local rubato = require("lib.rubato")
local anim_y = rubato.timed {
    rate = 60,
    easing = rubato.linear,
    intro = 0.1,
    duration = 0.5,
    awestore_compat = true
}

local anim_x = rubato.timed {
    rate = 60,
    easing = rubato.linear,
    intro = 0.1,
    duration = 0.5,
    awestore_compat = true 
}

local music_ncmp = bling.module.scratchpad { command = "xterm -name scratch_ncmp -e ncmpcpp",
                                      rule = { instance = "scratch_ncmp" },                 
                                      sticky = false, autoclose = true,                              
                                      floating = false, geometry = {x = 0 , y = 0},
                                      reapply = true, dont_focus_before_close  = true,
                                      --rubato = {x = anim_x, y = anim_y} 
}
local music_spot = bling.module.scratchpad { command = "spot_load",
                                      rule = { class = "Spotify" },                 
                                      sticky = false, autoclose = true,                              
                                      floating = false, geometry = {x = 0 , y = 0},
                                      reapply = true, dont_focus_before_close  = false,
                                      --rubato = {x = anim_x, y = anim_y} 
}
local file_manager = bling.module.scratchpad { command = "pcmanfm",
                                      rule = { instance = "pcmanfm" },                 
                                      sticky = false, autoclose = true,                              
                                      floating = false, geometry = {x = 0 , y = 0},
                                      reapply = true, dont_focus_before_close  = false,
                                      --rubato = {x = anim_x, y = anim_y} 
}
local discord = bling.module.scratchpad { command = "discord",
                                      rule = { class = "discord" },                 
                                      sticky = false, autoclose = true,                              
                                      floating = false, geometry = {x = 0 , y = 0},
                                      reapply = true, dont_focus_before_close  = false,
                                      --rubato = {x = anim_x, y = anim_y} 
}
local chromium = bling.module.scratchpad { command = "chromium",
                                      rule = { class = "Chromium" },                 
                                      sticky = false, autoclose = true,                              
                                      floating = false, geometry = {x = 0 , y = 0},
                                      reapply = true, dont_focus_before_close  = false,
                                      --rubato = {x = anim_x, y = anim_y} 
}

local scratchs = {music_ncmp,music_spot,file_manager,discord,chromium}
awesome.connect_signal("scratch::music", function() music_ncmp:toggle() end)
awesome.connect_signal("scratch::spot", function() music_spot:toggle() end)
awesome.connect_signal("scratch::filem", function() file_manager:toggle() end)
awesome.connect_signal("scratch::disco", function() discord:toggle() end)
awesome.connect_signal("scratch::brows", function() chromium:toggle() end)
awesome.connect_signal("scratch::turn_on", function()
    for _, scratch in ipairs(scratchs) do
        scratch:turn_on()
    end
end)
awesome.connect_signal("scratch::turn_off", function()
    for _, scratch in ipairs(scratchs) do
        scratch:turn_off()
    end
end)
