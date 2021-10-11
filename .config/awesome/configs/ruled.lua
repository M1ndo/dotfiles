local awful = require('awful')
local ruled = require('ruled')
local beautiful = require('beautiful')

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

    {
       rule_any = { type = { "dialog", "normal" } },
       properties = { titlebars_enabled = false }
    },

    {
       rule_any = {
          class = {"Google Chrome","Chromium","Firefox","Librewolf"}
       },
       properties = {
          screen = 1,
          tag = awful.util.tagnames[1]
       }
    },

    {
       rule_any = {
          class = { "pcmanfm" }
       },
       properties = {
          screen = 1,
          tag = awful.util.tagnames[2]
       }
    },

    {
       id = "Screen 1 Tag 3",
       rule_any = {
          class = {"Emacs", "lite"}
       },
       properties = {
          screen = 1,
          tag =  awful.util.tagnames[3]
       }
    },

    {
       rule_any = {
          class = {"stremio", "mpv", "vlc"}
       },
       properties = {
          screen = 1,
          tag =  awful.util.tagnames[4]
       }
    },

    {
       rule_any = {
          class = {"discord", "irssi"}
       },
       properties = {
          screen = 1,
          tag =  awful.util.tagnames[5]
       }
    },

    {
       rule_any = {
          class = {"spotify", "scratch_ncmp"}
       },
       properties = {
          screen = 1,
          tag =  awful.util.tagnames[6]
       }
    },

    {
       rule_any =  {
          class = {"Gimp", "VirtualBox Manager", "VirtualBox Machine", "Vmware"}
       },
       properties = {
          maximized = true,
       }
    },

    {
       rule_any = {
          instance = { "DTA", "copyq" },
          class = {"Arandr", "Blueberry", "Galculator", "Gnome-font-viewer", "Gpick", "Imagewriter", "Font-manager", "MessageWin", "Peek", "System-config-printer.py", "Sxiv", "feh", "qeh", "Unetbootin.elf", "Wpa_gui", "pinentry", "xtightvncviewer"},
          name = {"Event Tester"},
          role = {"AlarmWindow", "pop-up", "Preferences", "setup",
          }
       },
       properties = {
          floating = true,
       }
    }
}
