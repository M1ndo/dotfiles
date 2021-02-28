--[[
    Fallen_rainbow Awesome WM Theme 1.0
    (C) Ybenel <github.com/r2dr0dn/fallen_rainbow>
    inspired from github.com/lcpz (awesome-copycats 'rainbow')
--]]

local gears = require("gears")
local naughty = require('naughty')
local lain  = require("lain")
local awful = require("awful")
local wibox = require("wibox")
local dpi   = require("beautiful.xresources").apply_dpi
-- Ybenel.Bar.Mocp    = require("widgets.toggles")

local os = os
local my_table = awful.util.table or gears.table -- 4.{0,1} compatibility

local theme                                     = {}
theme.default_dir                               = require("awful.util").get_themes_dir() .. "default"
theme.dir                                       = os.getenv("HOME") .. "/.config/awesome/themes/fallen_rainbow"
theme.wallpaper                                 = theme.dir .. "/wall.png"
theme.font                                      = "scientifica 9"
theme.font2                                     = "Mononoki Nerd Font 9"
theme.fg_normal                                 = "#9E9E9E"
theme.fg_focus                                  = "#EBEBFF"
theme.bg_normal                                 = "#282C34"
theme.bg_focus                                  = "#242424"
theme.fg_urgent                                 = "#000000"
theme.bg_urgent                                 = "#FFFFFF"
theme.border_width                              = dpi(1)
theme.border_normal                             = "#242424"
theme.border_focus                              = "#EBEBFF"
theme.taglist_fg_focus                          = "#8DC702"
theme.taglist_bg_focus                          = "#242424"
theme.menu_height                               = dpi(16)
theme.menu_width                                = dpi(140)
theme.ocol                                      = "<span color='" .. theme.fg_normal .. "'>"
theme.tasklist_sticky                           = theme.ocol .. "[S]</span>"
theme.tasklist_ontop                            = theme.ocol .. "[T]</span>"
theme.tasklist_floating                         = theme.ocol .. "[F]</span>"
theme.tasklist_maximized_horizontal             = theme.ocol .. "[M] </span>"
theme.tasklist_maximized_vertical               = ""
theme.tasklist_disable_icon                     = true
theme.awesome_icon                              = theme.dir .."/icons/awesome.png"
theme.menu_submenu_icon                         = theme.dir .."/icons/submenu.png"
theme.taglist_squares_sel                       = theme.dir .. "/icons/square_sel.png"
theme.taglist_squares_unsel                     = theme.dir .. "/icons/square_unsel.png"
theme.useless_gap                               = dpi(6)
theme.layout_txt_tile                           = "[t]"
theme.layout_txt_tileleft                       = "[l]"
theme.layout_txt_tilebottom                     = "[b]"
theme.layout_txt_tiletop                        = "[tt]"
theme.layout_txt_fairv                          = "[fv]"
theme.layout_txt_fairh                          = "[fh]"
theme.layout_txt_spiral                         = "[s]"
theme.layout_txt_dwindle                        = "[d]"
theme.layout_txt_max                            = "[m]"
theme.layout_txt_fullscreen                     = "[F]"
theme.layout_txt_magnifier                      = "[M]"
theme.layout_txt_floating                       = "[*]"
theme.widget_mem                                = theme.dir .. "/icons/mem.png"
theme.widget_cpu                                = theme.dir .. "/icons/cpu.png"
theme.widget_temp                               = theme.dir .. "/icons/temp.png"
theme.titlebar_close_button_normal              = theme.default_dir.."/titlebar/close_normal.png"
theme.titlebar_close_button_focus               = theme.default_dir.."/titlebar/close_focus.png"
theme.titlebar_minimize_button_normal           = theme.default_dir.."/titlebar/minimize_normal.png"
theme.titlebar_minimize_button_focus            = theme.default_dir.."/titlebar/minimize_focus.png"
theme.titlebar_ontop_button_normal_inactive     = theme.default_dir.."/titlebar/ontop_normal_inactive.png"
theme.titlebar_ontop_button_focus_inactive      = theme.default_dir.."/titlebar/ontop_focus_inactive.png"
theme.titlebar_ontop_button_normal_active       = theme.default_dir.."/titlebar/ontop_normal_active.png"
theme.titlebar_ontop_button_focus_active        = theme.default_dir.."/titlebar/ontop_focus_active.png"
theme.titlebar_sticky_button_normal_inactive    = theme.default_dir.."/titlebar/sticky_normal_inactive.png"
theme.titlebar_sticky_button_focus_inactive     = theme.default_dir.."/titlebar/sticky_focus_inactive.png"
theme.titlebar_sticky_button_normal_active      = theme.default_dir.."/titlebar/sticky_normal_active.png"
theme.titlebar_sticky_button_focus_active       = theme.default_dir.."/titlebar/sticky_focus_active.png"
theme.titlebar_floating_button_normal_inactive  = theme.default_dir.."/titlebar/floating_normal_inactive.png"
theme.titlebar_floating_button_focus_inactive   = theme.default_dir.."/titlebar/floating_focus_inactive.png"
theme.titlebar_floating_button_normal_active    = theme.default_dir.."/titlebar/floating_normal_active.png"
theme.titlebar_floating_button_focus_active     = theme.default_dir.."/titlebar/floating_focus_active.png"
theme.titlebar_maximized_button_normal_inactive = theme.default_dir.."/titlebar/maximized_normal_inactive.png"
theme.titlebar_maximized_button_focus_inactive  = theme.default_dir.."/titlebar/maximized_focus_inactive.png"
theme.titlebar_maximized_button_normal_active   = theme.default_dir.."/titlebar/maximized_normal_active.png"
theme.titlebar_maximized_button_focus_active    = theme.default_dir.."/titlebar/maximized_focus_active.png"

-- Menu Icons
theme.browser_ico = theme.dir .. "/icons/browser.png"
theme.stremio_ico = theme.dir .. "/icons/stremio.png"
theme.gimp_ico = theme.dir .. "/icons/gimp.png"
theme.atom_ico = theme.dir .. "/icons/atom.png"
theme.telegram_ico = theme.dir .. "/icons/telegram.png"
theme.terminal_ico = theme.dir .. "/icons/xterm.png"
theme.discord_ico = theme.dir .. "/icons/discord.png"
theme.logout_ico = theme.dir .. "/icons/logout.png"
theme.sleep_ico = theme.dir .. "/icons/sleep.png"
theme.restart_ico = theme.dir .. "/icons/restart.png"
theme.exit_ico = theme.dir .. "/icons/shutdown.png"
theme.hibernate_ico = theme.dir .. "/icons/hibernate.png"

-- Bar Icons
theme.ghost_on = theme.dir .. "icons/ghost.svg"
theme.terex_off = theme.dir .. "icons/offside.svg"

-- lain related
theme.layout_txt_cascade                        = "[cascade]"
theme.layout_txt_cascadetile                    = "[cascadetile]"
theme.layout_txt_centerwork                     = "[centerwork]"
theme.layout_txt_termfair                       = "[termfair]"
theme.layout_txt_centerfair                     = "[centerfair]"

local markup = lain.util.markup
local white  = theme.fg_focus
local gray   = theme.fg_normal

-- Textclock
local mytextclock = wibox.widget.textclock(markup(white, " %H:%M "))
mytextclock.font = theme.font2

-- Calendar
theme.cal = lain.widget.cal({
    attach_to = { mytextclock },
    notification_preset = {
        font = "Mononoki Nerd Font 11",
        fg   = white,
        bg   = theme.bg_normal
    }
})

-- Mail IMAP check
--[[ commented because it needs to be set before use
theme.mail = lain.widget.imap({
    timeout  = 180,
    server   = "server",
    mail     = "mail",
    password = "keyring get mail",
    settings = function()
        mail_notification_preset.fg = white

        mail  = ""
        count = ""

        if mailcount > 0 then
            mail = "Mail "
            count = mailcount .. " "
        end

        widget:set_markup(markup.font(theme.font, markup(gray, mail) .. markup(white, count)))
    end
})
--]]

-- MPD
theme.mpd = lain.widget.mpd({
    settings = function()
        mpd_notification_preset.fg = white

        artist = mpd_now.artist .. " "
        title  = mpd_now.title  .. " "

        if mpd_now.state == "pause" then
            artist = "mpd "
            title  = "paused "
        elseif mpd_now.state == "stop" then
            artist = ""
            title  = ""
        end
        widget:set_markup(markup.font(theme.font, markup(gray, artist) .. markup(white, title)))
    end
})

-- /home fs
--[[ commented because it needs Gio/Glib >= 2.54
theme.fs = lain.widget.fs({
    notification_preset = { fg = white, bg = theme.bg_normal, font = theme.font },
    settings  = function()
        local fs_header, fs_p = "", ""

        if fs_now["/home"].percentage >= 90 then
            fs_header = " Hdd "
            fs_p      = fs_now["/home"].percentage
        end

        widget:set_markup(markup.font(theme.font, markup(gray, fs_header) .. markup(white, fs_p)))
    end
})
--]]

-- ALSA volume bar
theme.volume = lain.widget.alsabar({
    ticks = true, width = dpi(67),
    notification_preset = { font = theme.font }
})
theme.volume.tooltip.wibox.fg = theme.fg_focus
theme.volume.tooltip.wibox.font = theme.font2
theme.volume.bar:buttons(my_table.join (
          awful.button({}, 1, function()
            awful.spawn(string.format("%s -e alsamixer", terminal))
          end),
          awful.button({}, 2, function()
            os.execute(string.format("%s set %s 100%%", theme.volume.cmd, theme.volume.channel))
            theme.volume.update()
          end),
          awful.button({}, 3, function()
            os.execute(string.format("%s set %s toggle", theme.volume.cmd, theme.volume.togglechannel or theme.volume.channel))
            theme.volume.update()
          end),
          awful.button({}, 4, function()
            os.execute(string.format("%s set %s 1%%+", theme.volume.cmd, theme.volume.channel))
            theme.volume.update()
          end),
          awful.button({}, 5, function()
            os.execute(string.format("%s set %s 1%%-", theme.volume.cmd, theme.volume.channel))
            theme.volume.update()
          end)
))
local volumebg = wibox.container.background(theme.volume.bar, "#585858", gears.shape.rectangle)
local volumewidget = wibox.container.margin(volumebg, dpi(7), dpi(7), dpi(5), dpi(5))


-- MOC
local love_mc = wibox.widget.textbox(markup.font(theme.font2, markup("#ff006a",'♥ ')))
local prev_next_mc = wibox.widget.textbox(markup.font(theme.font2, markup('#00ffff', '  ')))

theme.moc = lain.widget.contrib.moc({
  settings = function()
    moc_notification_preset.fg = white
    artist = moc_now.artist .. " "
    title = moc_now.title .. ""
    if moc_now.state == "PAUSE" then
      artist = "Moc "
      title  = "Paused"
    elseif moc_now.state == "STOP" then
      artist = ""
      title = "Nothing To Play"
    elseif moc_now.state == "N/A" then 
      artist = ""
      title = ""
    end
    widget:set_markup(markup.font(theme.font, markup(gray, artist) .. markup(white, title)))
  end
})
love_mc:buttons(my_table.join(awful.button({ }, 2,
function ()
  os.execute('mocp -G ; sp next')
  theme.moc.update()
end)))
prev_next_mc:buttons(my_table.join(awful.button({}, 1,
function ()
  os.execute('mocp -f ; sp prev')
  theme.moc.update()
end)))

theme.spot = lain.widget.contrib.spot({
  settings = function()
    if spot_now.state == "Playing" then
      artist = spot_now.artist .. " "
      title = spot_now.title .. " "
    elseif spot_now.state == "Paused" then
      artist = "Spot "
      title  = "Paused"
    elseif spot_now.state == "Stopped" then
      artist = ""
      title  = ""
    end
    widget:set_markup(markup.font(theme.font, markup(gray, artist) .. markup(white, title)))
  end
})

-- prev_next_mc:buttons(my_table.join(awful.button({}, 3,
-- function ()
--   os.execute('mocp -r')
--   theme.moc.update()
-- end)))
-- Weather
theme.weather = lain.widget.weather({
    city_id = 2643743, -- placeholder (London)
    notification_preset = { font = theme.font, fg = white }
})

-- MEM
local memicon = wibox.widget.imagebox(theme.widget_mem)
local mem = lain.widget.mem({
    settings = function()
        widget:set_markup(markup.font(theme.font2, markup("#c6dd13", "") .. " " .. markup(gray, mem_now.used) .. "MB "))
    end
})

-- CPU
local cpuicon = wibox.widget.imagebox(theme.widget_cpu)
local cpu = lain.widget.cpu({
    settings = function()
        widget:set_markup(markup.font(theme.font2, markup("#16c982", "") .. " " .. markup(gray, cpu_now.usage) .. "% "))
    end
})

local temp = lain.widget.temp({
    settings = function()
        widget:set_markup(markup.font(theme.font2, markup("#e81010", "") .. " " .. markup(gray, coretemp_now) .. "°C "))
    end
})
local tempicon = wibox.widget.imagebox(theme.widget_temp)

local mem_wid = wibox.container.margin(wibox.widget {mem.widget, layout = wibox.layout.align.horizontal }, 2, 3), "#7197E7"
local cpu_wid = wibox.container.margin(wibox.widget {cpu.widget, layout = wibox.layout.align.horizontal }, 3, 4), "#A77AC4"
local temp_wid = wibox.container.margin(wibox.widget {temp.widget, layout = wibox.layout.align.horizontal }, 4, 4), "#7197E7"
local volum = wibox.widget.textbox(markup.font(theme.font2, markup("#9517b5",' ')))
-- Separators
local first = wibox.widget.textbox(markup.font("Terminus 4", " "))
local spr   = wibox.widget.textbox(' ')

local function update_txt_layoutbox(s)
    -- Writes a string representation of the current layout in a textbox widget
    local txt_l = theme["layout_txt_" .. awful.layout.getname(awful.layout.get(s))] or ""
    s.mytxtlayoutbox:set_text(txt_l)
end

function theme.at_screen_connect(s)
    -- Quake application
    s.quake = lain.util.quake({ app = awful.util.terminal })

    -- If wallpaper is a function, call it with the screen
    local wallpaper = theme.wallpaper
    if type(wallpaper) == "function" then
        wallpaper = wallpaper(s)
    end
    gears.wallpaper.maximized(wallpaper, s, true)

    -- Tags
    awful.tag(awful.util.tagnames, s, awful.layout.layouts)

    -- Create a promptbox for each screen
    s.mypromptbox = awful.widget.prompt()

    -- Textual layoutbox
    s.mytxtlayoutbox = wibox.widget.textbox(theme["layout_txt_" .. awful.layout.getname(awful.layout.get(s))])
    awful.tag.attached_connect_signal(s, "property::selected", function () update_txt_layoutbox(s) end)
    awful.tag.attached_connect_signal(s, "property::layout", function () update_txt_layoutbox(s) end)
    s.mytxtlayoutbox:buttons(my_table.join(
                           awful.button({}, 1, function() awful.layout.inc(1) end),
                           awful.button({}, 2, function () awful.layout.set( awful.layout.layouts[1] ) end),
                           awful.button({}, 3, function() awful.layout.inc(-1) end),
                           awful.button({}, 4, function() awful.layout.inc(1) end),
                           awful.button({}, 5, function() awful.layout.inc(-1) end)))

    -- Create a taglist widget
    s.mytaglist = awful.widget.taglist(s, awful.widget.taglist.filter.all, awful.util.taglist_buttons)

    -- Create a tasklist widget
    s.mytasklist = awful.widget.tasklist(s, awful.widget.tasklist.filter.currenttags, awful.util.tasklist_buttons)

    -- Create the wibox
    s.mywibox = awful.wibar({ position = "top", screen = s, height = dpi(18), bg = theme.bg_normal, fg = theme.fg_normal, opacity = 0.86 })

    -- Add widgets to the wibox
    s.mywibox:setup {
        layout = wibox.layout.align.horizontal,
        { -- Left widgets
            layout = wibox.layout.fixed.horizontal,
            first,
            s.mytaglist,
            spr,
            -- s.mytxtlayoutbox,
            --spr,
            s.mypromptbox,
            spr,
            love_mc,
            theme.spot,
            theme.moc,
            prev_next_mc,
        },
        spr,
        -- s.mytasklist, -- Middle widget
        { -- Right widgets
            layout = wibox.layout.fixed.horizontal,
            wibox.widget.systray(),
            spr,
            --theme.mpd.widget,
            --theme.mail.widget,
            -- theme.fs.widget,
            cpu_wid,
            mem_wid,
            temp_wid,
            volum,
            volumewidget,
            mytextclock,
        },
    }
end

return theme
