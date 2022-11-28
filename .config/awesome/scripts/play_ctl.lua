local awful = require('awful')
local gears = require('gears')
local wibox = require('wibox')
local helpers = require('lain.helpers')
local dpi   = require("beautiful").xresources.apply_dpi
local xrdb = require("beautiful.xresources").get_current_theme()
local awesome, client, mouse= awesome, client, mouse

--- Local Variable
local dash_width = dpi(500)
local dash_gap = dpi(10)
local mfont = "Ubuntu Mono "
local album_art = wibox.widget.imagebox()
local music = album_art
music.visible = true

local musictooltip = awful.tooltip {}
musictooltip.shape = helpers.prrect(dpi(6) - 3, false, true, true, false)
musictooltip.preferred_alignments = {"middle", "front", "back"}
musictooltip.mode = "outside"
musictooltip:add_to_object(music)
musictooltip.text = "Not updated"

awesome.connect_signal("bling::playerctl::status", function(playing) music.visible = playing end)
awesome.connect_signal("bling::playerctl::player_stopped", function() music.visible = false end)
awesome.connect_signal("bling::playerctl::title_artist_album", function(title, artist, album_path)
                           musictooltip.markup = tostring(title) .. " - " .. tostring(artist)
                           music.visible = true
                           album_art:set_image(gears.surface.load_uncached_silently(album_path))
                           album_art:emit_signal("widget::redraw_needed")
end)
album_art:buttons(gears.table.join(
                  awful.button({}, 1, function()
                          awful.spawn("playerctl play-pause")
                  end),
                  awful.button({}, 4, function()
                          awful.spawn("playerctl next")
                  end),
                  awful.button({}, 5, function()
                          awful.spawn("playerctl previous")
                  end)
))

local uparr = wibox.widget.textbox("")
uparr.align = "center"
uparr.valign = "center"
uparr.markup = helpers.colorize_text("", xrdb.color6)
local downarr = wibox.widget.textbox("")
downarr.align = "center"
downarr.valign = "center"
downarr.markup = helpers.colorize_text("", xrdb.foreground)

local musicbox = wibox.widget {
    {
        {
            uparr,
            helpers.vertical_pad(2),
            music,
            helpers.vertical_pad(2),
            downarr,
            layout = wibox.layout.fixed.vertical
        },
        top   = 5,
        bottom = 5,
        widget = wibox.container.margin
    },
    bg = xrdb.color0,
    widget = wibox.container.background
}


-- music progress bar

local progress_color = {
    type = 'linear',
    from = {0, 0},
    to = {dash_width, 0},
    stops = {
        {0, xrdb.color6},
        {1, xrdb.color5}
    }
}
local musicprogressbar = wibox.widget {
        max_value = 1,
        value = 0.4,
        forced_height = dpi(10),
        shape         = helpers.rrect(dpi(6) - 3),
        bar_shape         = helpers.rrect(dpi(6) - 3),
        forced_width = dash_width - 2 * dash_gap,
        shape_border_width = 0,
        background_color = "alpha",
        color = progress_color,
        widget = wibox.widget.progressbar,
}

local musicprogress = wibox.widget {
        {
                {
                        wibox.widget.textbox(""),
                        shape = helpers.rrect(dpi(6) - 3),
                        bg = xrdb.color8,
                        widget = wibox.container.background
                },
                margins = dpi(3),
                widget = wibox.container.margin
        },
        musicprogressbar,
        layout = wibox.layout.stack
}

awesome.connect_signal("bling::playerctl::position", function(pos, max, _) musicprogressbar.value = pos / max end)

-- music text

local text_title = wibox.widget.textbox("")
text_title.text = "_"
text_title.valign = "center"
text_title.align = "left"
text_title.font = mfont .. "20"

local text_artist = wibox.widget.textbox("")
text_artist.text = "_"
text_artist.valign = "center"
text_artist.align = "left"
text_artist.font = mfont .. "12"

awesome.connect_signal("bling::playerctl::title_artist_album", function(title, artist, _, _)
        text_title.markup = helpers.colorize_text(tostring(title), xrdb.color6)
        text_artist.markup = helpers.colorize_text(tostring(artist), xrdb.foreground)
end)

local musicinfo = wibox.widget {
        {
                layout = wibox.layout.fixed.vertical,
                text_title,
                text_artist,
        },
        top = dpi(15),
        bottom = dpi(15),
        left = dpi(25),
        widget = wibox.container.margin
}

---}}}
