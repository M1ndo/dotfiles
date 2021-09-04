local awful = require("awful")
local gears = require("gears")
local wibox = require("wibox")
local beautiful = require("beautiful")
local dpi   = require("beautiful").xresources.apply_dpi
local xrdb = require("beautiful.xresources").get_current_theme()
local awesome, mouse = awesome, mouse
local naughty = require("naughty")

local helpers = require("lain.helpers")
local bar_height = dpi(10)
local dash_width = dpi(400)
local dash_gap = dpi(100)
local mfont = "Ubuntu Mono "

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
        value = 0.782,
        forced_height = dpi(10),
        shape         = helpers.rrect(dpi(6) - 3),
        bar_shape         = helpers.rrect(dpi(6) - 3),
        forced_width = dash_width - 2 * dash_gap,
        shape_border_width = 0,
        background_color = "alpha",
        color = progress_color,
        widget = wibox.widget.progressbar,
}



local text_title = wibox.widget.textbox("")
text_title.text = "_"
text_title.valign = "center"
text_title.align = "centre"
text_title.font = mfont .. "12"

local text_artist = wibox.widget.textbox("")
text_artist.text = "_"
text_artist.valign = "center"
text_artist.align = "centre"
text_artist.font = mfont .. "12"

local album_art = wibox.widget.imagebox()
album_art.forced_height = 200
album_art.forced_width = 200
local music = album_art
music.visible = true


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


awesome.connect_signal("bling::playerctl::title_artist_album", function(title, artist, art_path, player_name)
        text_title.markup = helpers.colorize_text(tostring(title) .. " - ", xrdb.color6)
        text_artist.markup = helpers.colorize_text(tostring(artist), xrdb.foreground)
        music.visible = true
        album_art:set_image(gears.surface.load_uncached_silently(art_path))
        album_art:emit_signal("widget::redraw_needed")
end)
awesome.connect_signal("bling::playerctl::position", function(pos, max, _) musicprogressbar.value = tonumber(string.format("%.10f", pos/max) * 10000000) end)

local ply_pop = awful.popup {
        widget  = {
                {  
                        {
                                {
                                        {
                                                album_art,
                                                layout = wibox.layout.fixed.vertical,
                                        },
                                        align = "centre",
                                        widget = wibox.container.place
                                },
                                align = "centre",
                                widget = wibox.container.background,
                        },
                        layout = wibox.layout.fixed.vertical,
                        {
                                {
                                        {        
                                                text_title,
                                                text_artist,
                                                layout = wibox.layout.fixed.horizontal,
                                        },
                                        margins = dpi(5),
                                        widget = wibox.container.margin
                                },
                                bg = "#242424",
                                shape = helpers.rrect(dpi("2")),
                                widget = wibox.container.background,
                        },
                        layout = wibox.layout.fixed.vertical,
                        {
                                {
                                        {
                                                wibox.widget.textbox(""),
                                                bg = "#242424",
                                                shape = helpers.rrect(dpi(10)),
                                                widget = wibox.container.background
                                        },
                                        margins = bar_height / 4,
                                        widget = wibox.container.margin,
                                },
                                musicprogressbar,
                                layout = wibox.layout.stack,
                                
                        },
                },
                margins = dpi(0),
                widget = wibox.container.margin
        },
        bg = "#242424",
        y = (768 - 250),
        x = (1366 - 200) / 2,
        visible = false,
        ontop = true,
        shape = helpers.rrect(dpi(10)),
        hide_on_right_click = true,
}

local hide = gears.timer.start_new(5, function() ply_pop.visible = false end)

awesome.connect_signal("evil::plyctl", function()
        ply_pop.visible = true
        hide:again()
end)
