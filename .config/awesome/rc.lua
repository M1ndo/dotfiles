-- My Personall Config File
-- Customized by ybenel
-- Date: 10/02/2021
-- {{{  libraries
local awesome, client, mouse, screen, tag = awesome, client, mouse, screen, tag
local ipairs, string, os, table, tostring, tonumber, type = ipairs, string, os, table, tostring, tonumber, type

--https://awesomewm.org/doc/api/documentation/05-awesomerc.md.html
-- Standard awesome library
local gears         = require("gears") --Utilities such as color parsing and objects
local awful         = require("awful") --Everything related to window managment
                      require("awful.autofocus")
-- Widget and layout library
local wibox         = require("wibox")

-- Theme handling library
local beautiful     = require("beautiful")

-- Notification library
local naughty       = require("naughty")
naughty.config.defaults['icon_size'] = 100

--local menubar       = require("menubar")

local lain          = require("lain")
local freedesktop   = require("freedesktop")
-- Tabbed Layout
local leaved = require "awesome-leaved"
-- Enable hotkeys help widget for VIM and other apps
-- when client with a matching name is opened:
local hotkeys_popup = require("awful.hotkeys_popup").widget
                      require("awful.hotkeys_popup.keys")
local my_table      = awful.util.table or gears.table -- 4.{0,1} compatibility
-- }}}



-- {{{ Error handling
-- Check if awesome encountered an error during startup and fell back to
-- another config (This code will only ever execute for the fallback config)
if awesome.startup_errors then
    naughty.notify({ preset = naughty.config.presets.critical,
                     title = "Oops, there were errors during startup!",
                     text = awesome.startup_errors })
end

-- Handle runtime errors after startup
do
    local in_error = false
    awesome.connect_signal("debug::error", function (err)
        -- Make sure we don't go into an endless error loop
        if in_error then return end
        in_error = true

        naughty.notify({ preset = naughty.config.presets.critical,
                         title = "Oops, an error happened!",
                         text = tostring(err) })
        in_error = false
    end)
end
-- }}}



-- {{{ Autostart windowless processes
--local function run_once(cmd_arr)
--    for _, cmd in ipairs(cmd_arr) do
--        awful.spawn.with_shell(string.format("pgrep -u $USER -fx '%s' > /dev/null || (%s)", cmd, cmd))
--    end
--end

-- run_once({ "unclutter -root" }) -- entries must be comma-separated
-- }}}


-- {{{ Variable definitions

local themes = {
    "fallen_rainbow",
}

-- choose your theme here
local chosen_theme = themes[1]

local theme_path = string.format("%s/.config/awesome/themes/%s/theme.lua", os.getenv("HOME"), chosen_theme)
beautiful.init(theme_path)
-- Include Bling 
local bling = require("lib.bling")
require("scratchpads")
require("vol_pop")
require("temp")

-- Warn If Temperature
awesome.connect_signal("evil::temp", function(temp)
    if temp > 65 then
        naughty.notify {title="[!] CPU is getting hot",
                        text="Currently at " .. tostring(temp) .. "°C",icon=beautiful.temp_icon,fg="#b3ff1a"}
    end
    if temp > 79 then
        naughty.notify {title="[!] CPU is on fire",
                        text="Currently at " .. tostring(temp) .. "°C",icon=beautiful.temp_icon,bg="#ff0000"}
    end
end)

-- modkey or mod4 = super key
local modkey       = "Mod4"
local altkey       = "Mod1"
local modkey1      = "Control"

-- personal variables
--change these variables if you want
local browser           = "firefox"
local editor            = os.getenv("EDITOR") or "nvim"
local editorgui         = "xed"
local filemanager       = "pcmanfm"
local mailclient        = "evolution"
local mediaplayer       = "vlc"
local scrlocker         = "xscreensaver"
local terminal          = "xterm"
local virtualmachine    = "virtualbox"

-- awesome variables
local l = awful.layout.suit
local ll = leaved.layout.suit.tile
awful.util.terminal = terminal
awful.util.tagnames = { "", "", " ", "", " ", " "}
awful.layout.layouts = {
    l.tile,
    l.floating,
    l.spiral,
    bling.layout.mstab,
    lain.layout.termfair,
    ll.right,
    ll.left,
    ll.bottom, 
    ll.top,
}

awful.util.taglist_buttons = my_table.join(
    awful.button({ }, 1, function(t) t:view_only() end),
    awful.button({ modkey }, 1, function(t)
        if client.focus then
            client.focus:move_to_tag(t)
        end
    end),
    awful.button({ }, 3, awful.tag.viewtoggle),
    awful.button({ modkey }, 3, function(t)
        if client.focus then
            client.focus:toggle_tag(t)
        end
    end),
    awful.button({ }, 4, function(t) awful.tag.viewnext(t.screen) end),
    awful.button({ }, 5, function(t) awful.tag.viewprev(t.screen) end)
)

awful.util.tasklist_buttons = my_table.join(
    awful.button({ }, 1, function (c)
        if c == client.focus then
            c.minimized = true
        else
            c:emit_signal("request::activate", "tasklist", {raise = true})
        end
    end),
    awful.button({ }, 3, function ()
        local instance = nil

        return function ()
            if instance and instance.wibox.visible then
                instance:hide()
                instance = nil
            else
                instance = awful.menu.clients({theme = {width = 250}})
            end
        end
    end),
    awful.button({ }, 4, function () awful.client.focus.byidx(1) end),
    awful.button({ }, 5, function () awful.client.focus.byidx(-1) end)
)

lain.layout.termfair.nmaster           = 3
lain.layout.termfair.ncol              = 1
lain.layout.termfair.center.nmaster    = 3
lain.layout.termfair.center.ncol       = 1
lain.layout.cascade.tile.offset_x      = 2
lain.layout.cascade.tile.offset_y      = 32
lain.layout.cascade.tile.extra_padding = 5
lain.layout.cascade.tile.nmaster       = 5
lain.layout.cascade.tile.ncol          = 2

-- }}}



-- Menu Without apps

local myawesomemenu = {
    { "hotkeys", function() return false, hotkeys_popup.show_help end },
    { "manual", terminal .. " -e 'man awesome'" },
    { "edit config", terminal .. " -e nvim /home/ybenel/.config/awesome/rc.lua" },
    { "arandr", "arandr" },
    { "restart", awesome.restart },
}
beautiful.menu_font = "scientifica 9"
beautiful.menu_border_color = "#002133"
beautiful.menu_border_width = 2
mymainmenu = awful.menu({ items = { { "awesome", myawesomemenu, beautiful.awesome_icon },
                                    {"Browser", "chromium", beautiful.browser_ico},
                                    {"Stremio", "stremio", beautiful.stremio_ico},
				                    {"Pcmanfm", "pcmanfm", beautiful.pcman_ico},
                                    {"Lite", "lite", beautiful.atom_ico},
                                    {"Gimp","gimp", beautiful.gimp_ico},
                                    --{"Discord", "discord", beautiful.discord_ico},
                                    --{"Telegram", "telegram-desktop", beautiful.telegram_ico},
                                    { "Terminal", terminal, beautiful.terminal_ico},
                                    { "Log out", function() awesome.quit() end, beautiful.logout_ico},
                                    { "Sleep", "xscreensaver-command -lock", beautiful.sleep_ico},
                                    { "Hiber", "systemctl hibernate", beautiful.hibernate_ico},
                                    { "Restart", "systemctl reboot", beautiful.restart_ico},
                                    { "Exit", "shutdown now", beautiful.exit_ico},
                                  }
                        })

--mylauncher = awful.widget.launcher({ image = beautiful.awesome_icon,
 --                                    menu = mymainmenu })


-- {{{ Screen
-- Re-set wallpaper when a screen's geometry changes (e.g. different resolution)
screen.connect_signal("property::geometry", function(s)
    -- Wallpaper
    if beautiful.wallpaper then
        local wallpaper = beautiful.wallpaper
        -- If wallpaper is a function, call it with the screen
        if type(wallpaper) == "function" then
            wallpaper = wallpaper(s)
        end
        gears.wallpaper.maximized(wallpaper, s, true)
    end
end)
-- Create a wibox for each screen and add it
awful.screen.connect_for_each_screen(function(s) beautiful.at_screen_connect(s) end)
-- }}}



-- {{{ Mouse bindings
root.buttons(my_table.join(
    -- awful.button({ }, 3, function () awful.util.mymainmenu:toggle() end),
    awful.button({ }, 3, function () mymainmenu:toggle() end),
    awful.button({ }, 2, function () awful.spawn(terminal) end),
    awful.button({ }, 4, awful.tag.viewnext),
    awful.button({ }, 5, awful.tag.viewprev)
))
-- }}}



-- {{{ Key bindings
globalkeys = my_table.join(
    -- {{{ Personal keybindings
    --
    -- dmenu
    awful.key({ modkey, "Shift" }, "Return",
    function ()
        awful.spawn(string.format("dmenu_run -i  -nb '#212D32' -nf '#bbc5ff' -sb '#E37673' -sf '#292d3e' -fn 'scientifica:size=12'",
        beautiful.bg_normal, beautiful.fg_normal, beautiful.bg_focus, beautiful.fg_focus))
	end,
    {description = "show dmenu", group = "hotkeys"}),
    awful.key({ modkey, altkey }, "s",
    function ()
        awful.spawn(string.format("dmenu_run -c -l 10 -g 4 -b -p 'ybenel: ' -fn 'scientifica:size=12'",
        beautiful.bg_normal, beautiful.fg_normal, beautiful.bg_focus, beautiful.fg_focus))
	end,
    {description = "show dmenu (Small)", group = "hotkeys"}),
    awful.key({ altkey,  }, "Return",
    function()
	    awful.spawn(string.format("rofi -show drun -show-icons",
	    beautiful.bg_normal, beautiful.fg_normal, beautiful.bg_focus, beautiful.fg_focus))
	    end,
    {description = "show rofi", group = "hotkeys"}),
     
    -- Restore Minimized
    awful.key({ modkey, "Control" }, "n",
        function ()
            local c = awful.client.restore()
            -- Focus restored client
            if c then
                client.focus = c
                c:raise()
            end
        end,
        {description = "Restore Minimized", group = "Client"}),
    awful.key({ modkey, "Control", altkey }, "n",      
        function ()
            for _, cl in ipairs(mouse.screen.selected_tag:clients()) do
                local c = cl
                c:emit_signal(
                    "request::activate", "key.unminimize", {raise = true}
                )
            end
        end,
        {description = "Restore all Minimized windows in current tag", group = "client"}),

    -- My dmenu scripts (Alt+Ctrl+Key)
    awful.key({ altkey, "Control" }, "e", function () awful.util.spawn( "./.dmenu/dmenu-edit-configs.sh" ) end,
        {description = "Shortcut Editing Config" , group = "Dmenu Scripts" }),
    awful.key({ altkey, "Control" }, "h", function () awful.util.spawn( "./.dmenu/dmenu-sysmon.sh" ) end,
        {description = "System Monitoring" , group = "Dmenu Scripts" }),
    awful.key({ altkey, "Control" }, "s", function () awful.util.spawn( "./.dmenu/dmenu-scrot.sh" ) end,
        {description = "Scrot Screen" , group = "Dmenu Scripts" }),
    awful.key({ altkey, "Control" }, "p", function () awful.util.spawn( "passmenu" ) end,
        {description = "Passmenu" , group = "Dmenu Scripts" }),

    -- My applications (Super+Alt+Key)
    awful.key({ altkey, }, "e", function () awful.util.spawn( terminal .. " -e nvim" ) end,
        {description = "Open Nvim" , group = "hotkeys" }),
    awful.key({ modkey, }, "e", function () awful.util.spawn("emacsclient -c -a emacs") end,
        {description = "Open Emacs" , group = "hotkeys" }),
    awful.key({ modkey, altkey }, "c", function () awful.util.spawn( terminal.." -e mocp" ) end,
        {description = "mocp" , group = "terminal apps" }),
    awful.key({ modkey, altkey }, "e", function () awful.util.spawn( terminal.." -e irssi" ) end,
        {description = "Irssi" , group = "terminal apps" }),
    awful.key({ modkey, altkey }, "f", function () awful.util.spawn( terminal.." -e sh ./.config/vifm/scripts/vifmrun" ) end,
        {description = "vifm" , group = "terminal apps" }),
    awful.key({ modkey, altkey }, "l", function () awful.util.spawn( terminal.." -e lynx --cfg=~/.lynx/lynx.cfg --lss=~/.lynx/lynx.lss -vikeys https://ybenel.cf" ) end,
        {description = "lynx cli browser" , group = "terminal apps" }),
    awful.key({ modkey, modkey1 }, "e", function () awful.util.spawn( terminal.." -e 'torify irssi' " ) end,
        {description = "Torify Irssi" , group = "terminal apps" }),

    -- screenshots
    awful.key({ }, "Print", function () awful.util.spawn("scrot 'Ybenel_D-%Y-%m-%d_$wx$h.jpg' -e 'mv $f $$(xdg-user-dir PICTURES)'") end,
        {description = "Scrot FullScreen", group = "ScreenShots"}),
    awful.key({ modkey1, "Shift" }, "Print", function() awful.util.spawn("scrot -a $(slop -f '%x,%y,%w,%h') -d 2") end,
        {description = "Scrot Delayed Screen", group = "ScreenShots"}),

    -- Personal keybindings

    -- Scratchpads
    awful.key({ modkey, altkey }, "z", function () awesome.emit_signal("scratch::music") end,
        {description = "ncmpcpp" , group = "Scratchpad" }),
    awful.key({ }, "XF86Tools", function () awesome.emit_signal("scratch::spot") end,
        {description = "Spotify" , group = "Scratchpad" }),
    awful.key({ }, "XF86HomePage", function () awesome.emit_signal("scratch::brows") end,
        {description = "Chromium" , group = "Scratchpad" }),
    awful.key({ }, "XF86Mail", function () awesome.emit_signal("scratch::filem") end,
        {description = "File_Manager (Pcmanfm)" , group = "Scratchpad" }),
    awful.key({ modkey, }, "F1", function () awesome.emit_signal("scratch::disco") end,
        {description = "Discord" , group = "Scratchpad" }),
    awful.key({ modkey, }, "F12", function () awesome.emit_signal("scratch::turn_off") end,
        {description = "Turn Off Scratch Visibily" , group = "Scratchpad" }),
    awful.key({ modkey, }, "F9", function () awesome.emit_signal("scratch::turn_on") end,
        {description = "Turn On Scratch Visibily" , group = "Scratchpad" }),
    -- Mocp Controls    
    awful.key({ modkey, altkey }, "b", function () os.execute('mocp --toggle-pause') end,
        {description = "Moc Pause/Resume", group = "Moc"}),
    awful.key({ modkey, altkey }, "p", function () os.execute('mocp --play') end,
        {description = "Moc Play", group = "Moc"}),
    awful.key({ modkey, altkey }, "h", function () os.execute('mocp --previous') end,
        {description = "Moc Previous", group = "Moc"}),
    awful.key({ modkey, altkey }, "l", function () os.execute('mocp --next') end,
        {description = "Moc Next", group = "Moc"}),
    -- Mpd Controls
    awful.key({ modkey, altkey }, "x", function () os.execute('mpc toggle') end,
        {description = "Mpd Toggle", group = "Mpd"}),
    awful.key({ modkey, altkey }, "v", function () os.execute('mpc next') end,
        {description = "Mpd Next", group = "Mpd"}),
    awful.key({ modkey, altkey }, "b", function () os.execute('mpc prev') end,
        {description = "Mpd Previous", group = "Mpd"}),
    awful.key({ modkey, altkey }, "m", function () os.execute('mpc stop') end,
        {description = "Mpd Stop", group = "Mpd"}),

    -- Hotkeys Awesome

    awful.key({ modkey, "Shift" }, "s",      hotkeys_popup.show_help,
        {description = "show help", group="Awesome"}),
    awful.key({ modkey, }, "w", function () mymainmenu:toggle() end,
        {description = "show main menu", group = "awesome"}),
    -- Show/Hide Wibox
    awful.key({ modkey }, "b", function ()
            for s in screen do
                s.mywibox.visible = not s.mywibox.visible
                if s.mybottomwibox then
                    s.mybottomwibox.visible = not s.mybottomwibox.visible
                end
            end
        end,
        {description = "Toggle Wibox", group = "Awesome"}),
    -- Reload And Quit Awesome
    awful.key({ modkey, "Shift" }, "r", awesome.restart,
              {description = "Reload Awesome", group = "Awesome"}),
    awful.key({ modkey, "Shift" }, "q",  function () awesome.quit() end,
              {description = "Quit Awesome", group = "Awesome"}),
    -- Tag browsing with modkey
    awful.key({ modkey, modkey1 }, "Left",   awful.tag.viewprev,
        {description = "View Previous", group = "Tag"}),
    awful.key({ modkey, modkey1 }, "Right",  awful.tag.viewnext,
        {description = "View Next", group = "Tag"}),
    awful.key({ altkey, modkey1 }, "Tab", awful.tag.history.restore,
        {description = "Go Back", group = "Tag"}),

      -- Tag browsing alt + tab
    awful.key({ altkey, }, "Tab",   awful.tag.viewnext,
         {description = "View Next", group = "Tag"}),
    awful.key({ altkey, "Shift" }, "Tab",  awful.tag.viewprev,
         {description = "View Previous", group = "Tag"}),

     

    -- By direction client focus with arrows (Modkey + <UP/Down/Right/Left>)
    awful.key({ modkey, }, "Down",
        function()
            awful.client.focus.global_bydirection("down")
            bling.module.flash_focus.flashfocus(client.focus)
            if client.focus then client.focus:raise() end
        end,
        {description = "Focus Down", group = "Client"}),
    awful.key({ modkey, }, "Up",
        function()
            awful.client.focus.global_bydirection("up")
            bling.module.flash_focus.flashfocus(client.focus)
            if client.focus then client.focus:raise() end
        end,
        {description = "Focus Up", group = "Client"}),
    awful.key({ modkey, }, "Left",
        function()
            awful.client.focus.global_bydirection("left")
            bling.module.flash_focus.flashfocus(client.focus)
            if client.focus then client.focus:raise() end
        end,
        {description = "Focus Left", group = "Client"}),
    awful.key({ modkey, }, "Right",
        function()
            awful.client.focus.global_bydirection("right")
            bling.module.flash_focus.flashfocus(client.focus)
            if client.focus then client.focus:raise() end
        end,
        {description = "Focus Right", group = "Client"}),

    -- Non-empty tag browsing
    --awful.key({ modkey }, "Left", function () lain.util.tag_view_nonempty(-1) end,
              --{description = "view  previous nonempty", group = "tag"}),
   -- awful.key({ modkey }, "Right", function () lain.util.tag_view_nonempty(1) end,
             -- {description = "view  previous nonempty", group = "tag"}),


    -- Layout manipulation
    awful.key({ modkey, "Shift" }, "j", function () awful.client.swap.byidx(  1)    end,
              {description = "Swap With Next Client By Index", group = "Client"}),
    awful.key({ modkey, "Shift" }, "k", function () awful.client.swap.byidx( -1)    end,
              {description = "Swap With Previous Client By Index", group = "Client"}),
    awful.key({ modkey, }, ".", function () awful.screen.focus_relative( 1) end,
              {description = "Focus The pext Screen", group = "Screen"}),
    awful.key({ modkey, }, ",", function () awful.screen.focus_relative(-1) end,
              {description = "Focus The previous Screen", group = "Screen"}),
    awful.key({ modkey, }, "u", awful.client.urgent.jumpto,
              {description = "Jump To Urgent Client", group = "Client"}),
    awful.key({ modkey1, }, "Tab",
        function ()
            awful.client.focus.history.previous()
            if client.focus then
                client.focus:raise()
            end
        end,
        {description = "Go Back", group = "Client"}),


    -- On the fly useless gaps change
    awful.key({ altkey, "Control" }, "j", function () lain.util.useless_gaps_resize(1) end,
              {description = "Increment Useless Gaps", group = "Tag"}),
    awful.key({ altkey, "Control" }, "l", function () lain.util.useless_gaps_resize(-1) end,
              {description = "Decrement Useless Gaps", group = "Tag"}),

    -- Dynamic tagging
    awful.key({ modkey, "Shift" }, "n", function () lain.util.add_tag() end,
              {description = "Add New Tag", group = "Tag"}),
    awful.key({ modkey, "Control" }, "r", function () lain.util.rename_tag() end,
              {description = "Rename Tag", group = "Tag"}),
    awful.key({ modkey, "Shift" }, "Left", function () lain.util.move_tag(-1) end,
              {description = "Move Tag To The Left", group = "Tag"}),
    awful.key({ modkey, "Shift" }, "Right", function () lain.util.move_tag(1) end,
              {description = "Move Tag To The Right", group = "Tag"}),
    awful.key({ modkey, "Shift" }, "d", function () lain.util.delete_tag() end,
              {description = "Delete Tag", group = "Tag"}),

    -- Standard program
    awful.key({ modkey, }, "Return", function () awful.spawn(terminal) end,
              {description = "Launch Terminal", group = "Super"}),
    -- Leaved Layout 
    awful.key({ modkey, }, "s", leaved.keys.min_container,
              {description = "Minimize Container Windows", group = "Client"}),
    awful.key({ modkey, }, ";", leaved.keys.shiftOrder,
              {description = "Minimize Container Windows", group = "Client"}),
    awful.key({ modkey, }, "[", leaved.keys.splitH,
              {description = "Split Wind Horiz(Leaved Layout)", group = "Client"}),
    awful.key({ modkey, }, "]", leaved.keys.splitV,
              {description = "Split Wind Virtical(Leaved Layout)", group = "Client"}),
    awful.key({ modkey, }, "'", leaved.keys.shiftStyle,
              {description = "Change Style(Leaved Layout)", group = "Client"}),
     --- Pop Ups
    awful.key({ altkey, }, "i", function () awesome.emit_signal("evil::volume") end,
              {description = "Volume Pop Up", group = "Pop Ups"}),

    -- Tabbed Layout (Bling)
    awful.key({ altkey, }, ";", function() bling.module.tabbed.pop() end,
              {description = "Remove Focused Client From tabbed", group = "Client"}),
    awful.key({ altkey, }, "Left", function() bling.module.tabbed.iter(-1) end,
              {description = "Pick Client From Tabbed", group = "Client"}),
    awful.key({ altkey, }, "Right", function() bling.module.tabbed.iter(1) end,
              {description = "Pick Client From Tabbed", group = "Client"}),          
    awful.key({ altkey, }, "'", function() bling.module.tabbed.pick_with_dmenu() end,
              {description = "Iterates Through Focused Tabbing Group", group = "Client"}),
    
    -- Layout Selection
    awful.key({ modkey, }, "Tab", function () awful.layout.inc( 1) end,
              {description = "Select Next", group = "Layout"}),
    awful.key({ modkey, "Shift" }, "Tab", function () awful.layout.inc(-1) end,
              {description = "select previous", group = "layout"}),

    -- Dropdown application
    awful.key({ modkey, }, "z", function () awful.screen.focused().quake:toggle() end,
              {description = "Dropdown Terminal", group = "Super"}),

    -- Widgets popups
--     awful.key({ altkey, }, "n", function () lain.widget.cal.show(7)  end,
--               {description = "Show Calendar", group = "Widgets"}),
--     awful.key({ altkey, }, "h", function () if beautiful.fs then beautiful.fs.show(7) end end,
--               {description = "Show Filesystem", group = "Widgets"}),
    awful.key({ altkey, }, "w", function () if beautiful.weather then beautiful.weather.show(7) end end,
              {description = "Show Weather", group = "Widgets"}),

    -- Brightness
    awful.key({ }, "XF86MonBrightnessUp", function () os.execute("xbacklight -inc 10") end,
              {description = "+10%", group = "Hotkeys"}),
    awful.key({ }, "XF86MonBrightnessDown", function () os.execute("xbacklight -dec 10") end,
              {description = "-10%", group = "Hotkeys"}),

    -- ALSA volume control
    awful.key({ }, "XF86AudioRaiseVolume",
        function ()
            os.execute(string.format("amixer -q set %s 1%%+", beautiful.volume.channel))
            beautiful.volume.update()
        end),
    awful.key({ }, "XF86AudioLowerVolume",
        function ()
            os.execute(string.format("amixer -q set %s 1%%-", beautiful.volume.channel))
            beautiful.volume.update()
        end),
    awful.key({ }, "XF86AudioMute",
        function ()
            os.execute(string.format("amixer -q set %s toggle", beautiful.volume.togglechannel or beautiful.volume.channel))
            beautiful.volume.update()
        end),
    awful.key({ modkey1, "Shift" }, "m",
        function ()
            os.execute(string.format("amixer -q set %s 100%%", beautiful.volume.channel))
            beautiful.volume.update()
        end),
    awful.key({ modkey1, "Shift" }, "0",
        function ()
            os.execute(string.format("amixer -q set %s 0%%", beautiful.volume.channel))
            beautiful.volume.update()
        end),
    awful.key({altkey }, "l",
        function()
            os.execute('playerctl play')
        end),
    awful.key({altkey }, "p",
        function()
            os.execute('playerctl pause')
        end)
)

clientkeys = my_table.join(
    awful.key({ altkey, "Shift"   }, "m",      lain.util.magnify_client,
              {description = "Magnify Client", group = "Client"}),
    awful.key({ modkey,           }, "f", awful.client.floating.toggle,
        {description = "Toggle Floating", group = "Client"}),
 -- Kill Window 
    awful.key({ modkey, "Shift"   }, "c",      function (c) c:kill() end,
              {description = "Close", group = "Hotkeys"}),
    awful.key({ modkey, "Shift"   }, "a",
        function (c)
            for _, c in ipairs(mouse.screen.selected_tag:clients())
                do c:kill() 
            end
        end,
        {description = "Close All Windows", group = "Hotkeys"}),
        
    --awful.key({ modkey,           }, "t",      function (c) c.ontop = not c.ontop            end,
      --        {description = "Toggle To Keep On Top", group = "Client"}),
    
     -- Maximize / Fullscreen / Minimize Window
     awful.key({ modkey, }, "n",
        function (c)
            c.minimized = true
        end ,
        {description = "Minimize", group = "Client"}),
    
    awful.key({ modkey, altkey}, "n",
        function (c)
            for _, c in ipairs(mouse.screen.selected_tag:clients())
                do c.minimized = true
            end
        end ,
        {description = "Minimize", group = "Client"}),
              
    awful.key({ modkey, }, "m",
        function (c)
            c.maximized = not c.maximized
            c:raise()
        end ,
        {description = "Maximize", group = "Client"}),

    awful.key({ modkey, }, "space",
        function (c)
            c.maximized = not c.maximized
            c:raise()
        end ,
        {description = "Maximize", group = "Client"}),
    awful.key({ modkey, "Shift"   }, "space",
        function (c)
            c.fullscreen = not c.fullscreen
            c:raise()
        end,
        {description = "Toggle Fullscreen", group = "Client"})
)

-- Bind all key numbers to tags.
-- Be careful: we use keycodes to make it works on any keyboard layout.
-- This should map on the top row of your keyboard, usually 1 to 9.
for i = 1, 9 do
    -- Hack to only show tags 1 and 9 in the shortcut window (mod+s)
    local descr_view, descr_toggle, descr_move, descr_toggle_focus
    if i == 1 or i == 9 then
        descr_view = {description = "view tag #", group = "Tag"}
        descr_toggle = {description = "Toggle Tag #", group = "Tag"}
        descr_move = {description = "Move Focused Client To Tag #", group = "Tag"}
        descr_toggle_focus = {description = "Toggle Focused Client On Tag #", group = "Tag"}
    end
    globalkeys = my_table.join(globalkeys,
        -- View tag only.
        awful.key({ modkey }, "#" .. i + 9,
                  function ()
                        local screen = awful.screen.focused()
                        local tag = screen.tags[i]
                        if tag then
                           tag:view_only()
                        end
                  end,
                  descr_view),
        -- Toggle tag display.
        awful.key({ modkey, "Control" }, "#" .. i + 9,
                  function ()
                      local screen = awful.screen.focused()
                      local tag = screen.tags[i]
                      if tag then
                         awful.tag.viewtoggle(tag)
                      end
                  end,
                  descr_toggle),
        -- Move client to tag.
        awful.key({ modkey, "Shift" }, "#" .. i + 9,
                  function ()
                      if client.focus then
                          local tag = client.focus.screen.tags[i]
                          if tag then
                              client.focus:move_to_tag(tag)
                          end
                     end
                  end,
                  descr_move),
        -- Toggle tag on focused client.
        awful.key({ modkey, "Control", "Shift" }, "#" .. i + 9,
                  function ()
                      if client.focus then
                          local tag = client.focus.screen.tags[i]
                          if tag then
                              client.focus:toggle_tag(tag)
                          end
                      end
                  end,
                  descr_toggle_focus)
    )
end

clientbuttons = gears.table.join(
    awful.button({ }, 1, function (c)
        c:emit_signal("request::activate", "mouse_click", {raise = true})
    end),
    awful.button({ modkey }, 1, function (c)
        c:emit_signal("request::activate", "mouse_click", {raise = true})
        awful.mouse.client.move(c)
    end),
    awful.button({ modkey }, 3, function (c)
        c:emit_signal("request::activate", "mouse_click", {raise = true})
        awful.mouse.client.resize(c)
    end)
)

-- Set keys
root.keys(globalkeys)
-- }}}



-- {{{ Rules
-- Rules to apply to new clients (through the "manage" signal).
awful.rules.rules = {
    -- All clients will match this rule.
    { rule = { },
      properties = { border_width = beautiful.border_width,
                     border_color = beautiful.border_normal,
                     focus = awful.client.focus.filter,
                     raise = true,
                     keys = clientkeys,
                     buttons = clientbuttons,
                     screen = awful.screen.preferred,
                     placement = awful.placement.no_overlap+awful.placement.no_offscreen,
                     size_hints_honor = false
     }
    },

    -- Titlebars
    { rule_any = { type = { "dialog", "normal" } },
      properties = { titlebars_enabled = false } },

    -- Set applications to always map on the tag 1 on screen 1.
    -- find class or role via xprop command
    { rule = { name = "Google Chrome" },
      properties = { screen = 1, tag = awful.util.tagnames[1] } },
      
    { rule = { class = "Chromium" },
      properties = { screen = 1, tag = awful.util.tagnames[1] } },
  
    { rule = { class = "Firefox" },
      properties = { screen = 1, tag = awful.util.tagnames[1] } },
      
    { rule = { class = "LibreWolf" },
      properties = { screen = 1, tag = awful.util.tagnames[1] } },

    { rule = { class = "mpv" },
        properties = { screen = 1, tag = awful.util.tagnames[4] } },

    { rule = { class = "lite" },
        properties = { screen = 1, tag = awful.util.tagnames[3] } },

    { rule = { class = "vlc" },
        properties = { screen = 1, tag = awful.util.tagnames[4] } },

    { rule = { class = "Gimp" },
        properties = { screen = 1, tag = awful.util.tagnames[4] } },
    
    { rule = { class = "stremio" },
        properties = { screen = 1, tag = awful.util.tagnames[4] } },

    { rule = { class = "Vmware" },
        properties = { screen = 1, tag = awful.util.tagnames[4] } },
    -- Set applications to be maximized at startup.
    -- find class or role via xprop command

    { rule = { class = editorgui },
          properties = { maximized = true } },

    { rule = { class = "Gimp*", role = "gimp-image-window" },
          properties = { maximized = true } },

    { rule = { class = "inkscape" },
          properties = { maximized = true } },

    { rule = { class = mediaplayer },
          properties = { maximized = true } },

    { rule = { class = "Vlc" },
          properties = { maximized = true } },

    { rule = { class = "VirtualBox Manager" },
          properties = { maximized = true } },

    { rule = { class = "VirtualBox Machine" },
          properties = { maximized = true } },
          
    { rule = { class = "Vmware" },
          properties = { maximized = true } }, 

    { rule = { class = "Xfce4-settings-manager" },
          properties = { floating = false } },



    -- Floating clients.
    { rule_any = {
        instance = {
          "DTA",  -- Firefox addon DownThemAll.
          "copyq",  -- Includes session name in class.
        },
        class = {
          "Arandr",
          "Blueberry",
          "Galculator",
          "Gnome-font-viewer",
          "Gpick",
          "Imagewriter",
          "Font-manager",
          "Kruler",
          "MessageWin",  -- kalarm.
          "Oblogout",
          "Peek",
          "Skype",
          "System-config-printer.py",
          "Sxiv",
          "Unetbootin.elf",
          "Wpa_gui",
          "pinentry",
          "veromix",
          "xtightvncviewer"},

        name = {
          "Event Tester",  -- xev.
        },
        role = {
          "AlarmWindow",  -- Thunderbird's calendar.
          "pop-up",       -- e.g. Google Chrome's (detached) Developer Tools.
          "Preferences",
          "setup",
        }
      }, properties = { floating = true }},

}
-- }}}

-- {{{ Signals
-- Signal function to execute when a new client appears.
client.connect_signal("manage", function (c)
    -- Set the windows at the slave,
    -- i.e. put it at the end of others instead of setting it master.
    -- if not awesome.startup then awful.client.setslave(c) end

    if awesome.startup and
      not c.size_hints.user_position
      and not c.size_hints.program_position then
        -- Prevent clients from being unreachable after screen count changes.
        awful.placement.no_offscreen(c)
    end
end)

-- Add a titlebar if titlebars_enabled is set to true in the rules.
client.connect_signal("request::titlebars", function(c)
    -- Custom
    if beautiful.titlebar_fun then
        beautiful.titlebar_fun(c)
        return
    end

    -- Default
    -- buttons for the titlebar
    local buttons = my_table.join(
        awful.button({ }, 1, function()
            c:emit_signal("request::activate", "titlebar", {raise = true})
            awful.mouse.client.move(c)
        end),
        awful.button({ }, 3, function()
            c:emit_signal("request::activate", "titlebar", {raise = true})
            awful.mouse.client.resize(c)
        end)
    )

    awful.titlebar(c, {size = 21}) : setup {
        { -- Left
            awful.titlebar.widget.iconwidget(c),
            buttons = buttons,
            layout  = wibox.layout.fixed.horizontal
        },
        { -- Middle
            { -- Title
                align  = "center",
                widget = awful.titlebar.widget.titlewidget(c)
            },
            buttons = buttons,
            layout  = wibox.layout.flex.horizontal
        },
        { -- Right
            awful.titlebar.widget.floatingbutton (c),
            awful.titlebar.widget.maximizedbutton(c),
            awful.titlebar.widget.stickybutton   (c),
            awful.titlebar.widget.ontopbutton    (c),
            awful.titlebar.widget.closebutton    (c),
            layout = wibox.layout.fixed.horizontal()
        },
        layout = wibox.layout.align.horizontal
    }
end)

-- Enable sloppy focus, so that focus follows mouse.
client.connect_signal("mouse::enter", function(c)
    c:emit_signal("request::activate", "mouse_enter", {raise = true})
end)

-- No border for maximized clients
function border_adjust(c)
    if c.maximized then -- no borders if only 1 client visible
        c.border_width = 0
    elseif #awful.screen.focused().clients > 1 then
        c.border_width = beautiful.border_width
        c.border_color = beautiful.border_focus
    end
end

client.connect_signal("focus", border_adjust)
client.connect_signal("property::maximized", border_adjust)
client.connect_signal("unfocus", function(c) c.border_color = beautiful.border_normal end)


-- }}}

-- Autostart applications
awful.spawn.with_shell("~/.config/awesome/autostart.sh")
--awful.spawn.with_shell("xmp ~/mod_music/morning_ditty.mod")
