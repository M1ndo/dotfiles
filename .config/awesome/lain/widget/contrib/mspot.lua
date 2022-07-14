--[[

     Licensed under GNU General Public License v3
      (C) 2022, ybenel <http://github.com/m1ndo>

--]]

local helpers      = require("lain.helpers")
local awful        = require('awful')
local shell        = require("awful.util").shell
local focused      = require("awful.screen").focused
local escape_f     = require("awful.util").escape
local naughty      = require("naughty")
local wibox        = require("wibox")
local gears = require("gears")
local os           = os
local string       = string

-- Spotify Music Client
-- lain.widget.contrib.mspot

local function factory(args)
    local mspot          = { widget = wibox.widget.textbox() }
    local args          = args or {}
    local show_tooltip  = args.show_toolip or false
    local timeout       = args.timeout or 1
    local settings      = args.settings or function() end

    helpers.set_map("current playing track", nil)

    function mspot.update()
        helpers.async("getmusic --current", function(f)
            spot_now = {
                state   = "N/A",
                artist  = "",
                title   = "",
                album   = "N/A",
                album_artist = "N/A",
            }


            escaped = string.gsub(f, '&', '&amp;')
            spot_now.album, spot_now.album_artist, spot_now.artist, spot_now.title,spot_now.state = string.match(escaped, 'Album%s*(.*)\nAlbumArtist%s*(.*)\nArtist%s*(.*)\nTitle%s*(.*)\nStatus%s*(.*)\n')
            if string.match(escaped, 'Playing') then
              spot_now.state = "Playing"
            elseif string.match(escaped, 'Paused') then
              spot_now.state = "Paused"
            else
              spot_now.state = "Stopped"
            end
            widget = mspot.widget
            settings()

            if spot_now.state == "Playing" then
                if spot_now.title ~= helpers.get_map("current playing track") then
                    helpers.set_map("current playing track", spot_now.title)
                end
            elseif  spot_now.state ~= "Paused" then
                helpers.set_map("current playing track", nil)
            end
            --widget:connect_signal("button::press", function(_, _, _, button)
            --    if (button == 1) then
            --        awful.spawn("playerctl play", false)      -- left click
            --    elseif (button == 3) then
            --        awful.spawn("playerctl pause", false)  -- scroll up
            --    end
            --end)
            if show_tooltip then
                local spot_tooltip = awful.tooltip {
                    mode = 'outside',
                    preferred_positions = {'bottom'},
                    border_width = 1,
                    border_color = "#a9a9a9"
                }
                -- spot.update()
                spot_tooltip:add_to_object(widget)
                spot_tooltip:set_shape(gears.shape.rounded_rect)
                widget:connect_signal("mouse::leave", function() spot_tooltip.visible = false end)
                widget:connect_signal('mouse::enter', function()
                    spot_tooltip.markup = '<span foreground="#43cd80"><b>Album</b></span>: ' .. spot_now.album
                        .. '\n<span foreground="#436eee"><b>Artist</b></span>: ' .. spot_now.artist
                        .. '\n<span foreground="#ff7256"><b>Song</b></span>: ' .. spot_now.title
                        .. '\n<span foreground="#ff006a"><b>Status</b></span>: ' .. spot_now.state
                end)
            end
        end)
    end


    mspot.timer = helpers.newtimer("mspot", timeout, mspot.update, true, true)

    return mspot
end

return factory
