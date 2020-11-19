--[[

     Licensed under GNU General Public License v3
      (C) 2020, ybenel <http://github.com/r2dr0dn>

--]]

local helpers      = require("lain.helpers")
local awful        = require('awful')
local shell        = require("awful.util").shell
local focused      = require("awful.screen").focused
local escape_f     = require("awful.util").escape
local naughty      = require("naughty")
local wibox        = require("wibox")
local os           = os
local string       = string

-- Spotify Music Client
-- lain.widget.contrib.moc

local function factory(args)
    local spot          = { widget = wibox.widget.textbox() }
    local args          = args or {}
    local show_tooltip  = args.show_toolip or false
    local timeout       = args.timeout or 1
    local settings      = args.settings or function() end

    helpers.set_map("current playing track", nil)

    function spot.update()
        helpers.async("sp current && sp status", function(f)
            spot_now = {
                state   = "N/A",
                artist  = "",
                title   = "",
                album   = "N/A",
                album_artist = "N/A",
            }


            escaped = string.gsub(f, '&', '&amp;')
            spot_now.album, spot_now.album_artist, spot_now.artist, spot_now.title = string.match(escaped, 'Album%s*(.*)\nAlbumArtist%s*(.*)\nArtist%s*(.*)\nTitle%s*(.*)\n')
            if string.match(escaped, 'Playing') then
              spot_now.state = "Playing"
            elseif string.match(escaped, 'Paused') then
              spot_now.state = "Paused"
            else
              spot_now.state = "Stopped"
            end
            widget = spot.widget
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
                 }
                -- spot.update()
                spot_tooltip:add_to_object(widget)
                widget:connect_signal('mouse::enter', function()
                    spot_tooltip.markup = '<b>Album</b>: ' .. spot_now.album
                        .. '\n<b>Artist</b>: ' .. spot_now.artist
                        .. '\n<b>Song</b>: ' .. spot_now.title
                end)
            end
        end)
    end


    spot.timer = helpers.newtimer("spot", timeout, spot.update, true, true)

    return spot
end

return factory
