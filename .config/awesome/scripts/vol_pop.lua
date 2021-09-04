local awful = require("awful")
local gears = require("gears")
local wibox = require("wibox")
local beautiful = require("beautiful")
local dpi   = require("beautiful").xresources.apply_dpi
local awesome, mouse = awesome, mouse

local helpers = require("lain.helpers")

local bar_height = dpi(10)
local vol_bar = wibox.widget {
        max_value = 1,
        min_value = 1,
        value = 1,
        forced_height = bar_height,
        forced_width = dpi(250),
        bar_shape = helpers.rrect(dpi(3)),
        shape = helpers.rrect(dpi(3)),
        color = "#fbdf90",
        background_color = "alphe", --beautiful.xcolor8,
        widget = wibox.widget.progressbar
}

local pop = awful.popup {
        widget = {
                {
                        -- helpers.horizontal_pad(dpi(20)),
                        {
                                {
                                        {
                                                -- wibox.widget.imagebox(beautiful.awesome_icon),
                                                {
                                                        markup = helpers.colorize_text("墳", "#fbdf90"),
                                                        font = "Ubuntu Mono 12",
                                                        -- color = beautiful.xbg,
                                                        widget = wibox.widget.textbox,
                                                },
                                                wibox.widget.textbox("huhu"),
                                                bg = "#242424",
                                                shape = helpers.rrect(dpi("2")),
                                                widget = wibox.container.background,
                                        },
                                        margins = dpi(5),
                                        widget = wibox.container.margin
                                },
                                valign = "center",
                                halign = "center",
                                widget = wibox.container.place
                        },
                        {
                                {
                                        {
                                                {
                                                        {
                                                                wibox.widget.textbox(""),
                                                                bg = "#242424",
                                                                shape = helpers.rrect(dpi(10)),
                                                                widget = wibox.container.background
                                                        },
                                                        margins = bar_height / 4,
                                                        widget = wibox.container.margin
                                                },
                                                vol_bar,
                                                layout = wibox.layout.stack,
                                        },
                                        margins = dpi(14),
                                        widget = wibox.container.margin
                                },
                                bg = "#242424",
                                shape = helpers.rrect(dpi("2")),
                                widget = wibox.container.background,
                        },
                        -- helpers.horizontal_pad(dpi(20)),
                        layout = wibox.layout.fixed.horizontal
                },
                margins = dpi(0),
                widget = wibox.container.margin
        },
        bg = "#242424",
        y = (768 - 70),
        x = (1366 - 300) / 2,
        visible = false,
        ontop = true,
        shape = helpers.rrect(dpi(10)),
}

local hide = gears.timer.start_new(5, function() pop.visible = false end)

awesome.connect_signal("evil::volume", function()
        pop.visible = true
        hide:again()
end)
