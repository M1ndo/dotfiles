#!/usr/bin/env python
# Configured added / modified by ybenel (github.com/m1ndo)
# Modification Date: 07/18/2021
import os
import re
import socket
import subprocess
from libqtile import qtile, layout, bar, widget, hook
from libqtile.config import Click, Drag, Group, KeyChord, EzKey, Match, Screen
from libqtile.command import lazy
from libqtile.lazy import lazy

mod = "mod4" 
myTerm = "xterm"

keys = [
	EzKey("M-S-r", lazy.restart(),desc="Qtile Restart"),
	EzKey("M-S-q", lazy.shutdown(),desc="Qtile Quit"),
	EzKey("M-<Return>", lazy.spawn(myTerm),desc="Spawn Terminal"),
	EzKey("M-S-<Return>",lazy.spawn("dmenu_run -fn 'scientifica:size=11' -nb '#212D32' -nf     '#bbc5ff' -sb '#E37673' -sf '#292d3e' -p 'Ybenel:'"),desc="Spawn Dmenu"),
	EzKey("M-<space>", lazy.window.toggle_fullscreen(),desc="Toggle Fullscreen"),
	EzKey("M-<Tab>", lazy.next_layout(),desc="Toggle Next Layout"),
	EzKey("M-l", lazy.layout.shrink(),lazy.layout.decrease_nmaster(),desc="Shrink Size"),
	EzKey("M-h", lazy.layout.grow(),lazy.layout.increase_nmaster(),desc="Grow Size"),
	EzKey("M-<comma>", lazy.prev_screen()),
	EzKey("M-<period>", lazy.next_screen()),
	EzKey("M-S-c",lazy.window.kill(),desc="Kill Active Window"),
	EzKey("M-<Up>", lazy.layout.up(),desc="Focus Up"),
	EzKey("M-<Down>", lazy.layout.down(),desc="Focus Down"),
	EzKey("M-<Left>", lazy.layout.left(),desc="Focus Left"),
	EzKey("M-<Right>", lazy.layout.right(),desc="Focus Right"),
	EzKey("M-S-<Up>", lazy.layout.shuffle_up(),desc="Move Window Up"),
	EzKey("M-S-<Down>", lazy.layout.shuffle_down(),desc="Move Window Down"),
	EzKey("M-S-<Left>", lazy.layout.shuffle_left(),desc="Move Window Left"),
	EzKey("M-S-<Right>", lazy.layout.shuffle_right(),desc="Move Window Right"),
	EzKey("M-f", lazy.window.toggle_floating(),desc="Toggle Float"),
	EzKey("M-S-<space>", lazy.layout.flip(),desc="Flip Windows"),
	EzKey("M-S-n", lazy.layout.normalize(), desc='reset layout'),
	EzKey("M-m", lazy.layout.maximize(), desc="Maximize Size"),
	EzKey("M-b",lazy.hide_show_bar("top"), desc='Toggle Top Bar'),
	EzKey("M-n",lazy.window.toggle_minimize(), desc='Toggle Minimize'),
	# Applications
	EzKey("M-e",lazy.spawn("emacsclient -c -a emacs"),desc="Spawn Emacs"),
	EzKey("M-A-z", lazy.spawn(myTerm + " -e ncmpcpp")),
	EzKey("M-C-s", lazy.spawn("rofi -combi-modi run,drun -show combi -modi combi -show-icons -icon-theme 'Breeze' -display-combi 'ybenel: '")),
	EzKey("M-A-s", lazy.spawn("dmenu_run -c -bw 2 -l 10 -g 4 -p 'ybenel: ' -fn 'scientifica:size=12'")),
	EzKey("M-A-e", lazy.spawn(myTerm + " -e irssi")),
	EzKey("M-A-c", lazy.spawn(myTerm + " -e mocp")),
	EzKey("A-C-s", lazy.spawn("./.dmenu/dmenu-scrot.sh")),
	EzKey("A-C-h", lazy.spawn("./.dmenu/dmenu-sysmon.sh")),
	EzKey("A-C-e", lazy.spawn("./.dmenu/dmenu-edit-configs.sh")),
	EzKey("A-C-p", lazy.spawn("passmenu")),
	EzKey("<XF86AudioLowerVolume>", lazy.spawn("amixer set Master 5%- unmute")),
	EzKey("<XF86AudioRaiseVolume>", lazy.spawn("amixer set Master 5%+ unmute")),
	EzKey("<XF86AudioMute>", lazy.spawn("mpc stop")),	  
	EzKey("<XF86AudioNext>", lazy.spawn("mpc next")),
	EzKey("<XF86AudioPrev>", lazy.spawn("mpc prev")),
	EzKey("<XF86AudioPlay>", lazy.spawn("mpc toggle")),
    EzKey("A-S-x",lazy.spawn("mpc toggle")),
    EzKey("A-S-v",lazy.spawn("mpc next")),
    EzKey("A-S-b",lazy.spawn("mpc prev")),
    EzKey("A-S-m",lazy.spawn("mpc stop")),
    EzKey("A-S-p",lazy.spawn("mocp --play")),
    EzKey("A-S-l",lazy.spawn("mocp --next")),
    EzKey("A-S-h",lazy.spawn("mocp --previous")),
    EzKey("A-S-<space>",lazy.spawn("mocp --toggle-pause")),
]


group_names = [("", {'layout': 'monadtall'}),
               ("", {'layout': 'monadtall'}),
               ("", {'layout': 'monadtall'}),
               ("", {'layout': 'monadtall'}),
               ("", {'layout': 'monadtall'}),
               ("", {'layout': 'monadtall'})]

groups = [Group(name, **kwargs) for name, kwargs in group_names]

for i, (name, kwargs) in enumerate(group_names, 1):
	keys.append(EzKey("M-"+str(i),lazy.group[name].toscreen()))
	keys.append(EzKey("M-S-"+str(i),lazy.window.togroup(name)))

layout_theme = {"border_width": 2,
                "margin": 8,
                "border_focus": "e1acff",
                "border_normal": "1D2330"
                }

layouts = [
    layout.MonadWide(**layout_theme),
    layout.Bsp(**layout_theme),
    layout.MonadTall(**layout_theme),
    layout.RatioTile(**layout_theme),
    layout.TreeTab(
         font = "Ubuntu",
         fontsize = 10,
         sections = ["Tabs"],
         section_fontsize = 10,
         border_width = 2,
         bg_color = "1c1f24",
         active_bg = "c678dd",
         active_fg = "000000",
         inactive_bg = "a9a1e1",
         inactive_fg = "1c1f24",
         padding_left = 0,
         padding_x = 0,
         padding_y = 5,
         section_top = 10,
         section_bottom = 20,
         level_shift = 8,
         vspace = 3,
         panel_width = 180
         ),
    layout.Floating(**layout_theme)
]

colors = [["#220b40", "#220b40"], # panel background
          ["#3d3f4b", "#434758"], # background for current screen tab
          ["#ffffff", "#ffffff"], # font color for group names
          ["#ff5555", "#ff5555"], # border line color for current tab
          ["#1d2d59", "#1d2d59"], # border line color for 'other tabs' and color for 'odd widgets'
          ["#568054", "#568054"], # color for the 'even widgets'
          ["#e1acff", "#e1acff"], # window name
          ["#ecbbfb", "#ecbbfb"]] # backbround for inactive screens

prompt = "{0}@{1}: ".format(os.environ["USER"], socket.gethostname())

##### DEFAULT WIDGET SETTINGS #####
widget_defaults = dict(
    font="Ubuntu Mono",
    fontsize = 12,
    padding = 2,
    background=colors[2]
)
extension_defaults = widget_defaults.copy()

def init_widgets_list():
    widgets_list = [
              widget.Sep(
                       linewidth = 0,
                       padding = 6,
                       foreground = colors[2],
                       background = colors[0]
                       ),
              widget.Image(
                       filename = "~/.config/qtile/icons/face-devilish.png",
                       scale = "False",
                       background = colors[0],
                       mouse_callbacks = {'Button1': lambda: qtile.cmd_spawn(myTerm)}
                       ),
             widget.Sep(
                       linewidth = 0,
                       padding = 6,
                       foreground = colors[2],
                       background = colors[0]
                       ),
              widget.GroupBox(
                       font = "Ubuntu Bold",
                       fontsize = 9,
                       margin_y = 3,
                       margin_x = 0,
                       padding_y = 5,
                       padding_x = 3,
                       borderwidth = 3,
                       active = colors[2],
                       inactive = colors[7],
                       rounded = False,
                       highlight_color = colors[1],
                       highlight_method = "line",
                       this_current_screen_border = colors[6],
                       this_screen_border = colors [4],
                       other_current_screen_border = colors[6],
                       other_screen_border = colors[4],
                       foreground = colors[2],
                       background = colors[0]
                       ),
              widget.Prompt(
                       prompt = prompt,
                       font = "Ubuntu Mono",
                       padding = 10,
                       foreground = colors[3],
                       background = colors[1]
                       ),
              widget.Sep(
                       linewidth = 0,
                       padding = 14,
                       foreground = colors[2],
                       background = colors[0]
                       ),
              widget.WindowName(
                       foreground = colors[6],
                       background = colors[0],
                       padding = 0
                       ),
              widget.Sep(
											linewidth = 0,
											padding = 10,
											foreground = colors[2],
											background = colors[0]
											),
							widget.Mpd2(
											background = colors[0],
											foreground = colors[2],
											#colors_progress = colors[2]
											no_connection = "Mpd Is Off",
											status_format = "{artist} - {title}",
											update_interval = 0.5
											),
              widget.Systray(
                       background = colors[0],
                       padding = 5
                       ),
              widget.Sep(
                       linewidth = 0,
                       padding = 6,
                       foreground = colors[0],
                       background = colors[0]
                       ),
              widget.TextBox(
                       text = '',
                       background = colors[0],
                       foreground = colors[5],
                       padding = 0,
                       fontsize = 37
                       ),
              widget.TextBox(
                       text = "🌡",
                       padding = 2,
                       foreground = colors[2],
                       background = colors[5],
                       fontsize = 11
                       ),
              widget.ThermalSensor(
                       foreground = colors[2],
                       background = colors[5],
                       threshold = 90,
                       padding = 5
                       ),
              widget.TextBox(
                       text='',
                       background = colors[5],
                       foreground = colors[4],
                       padding = 0,
                       fontsize = 37
                       ),
              widget.TextBox(
                       text = " 🖬",
                       foreground = colors[2],
                       background = colors[4],
                       padding = 0,
                       fontsize = 14
                       ),
              widget.Memory(
                       foreground = colors[2],
                       background = colors[4],
                       padding = 5
                       ),
              widget.TextBox(
                       text = '',
                       background = colors[4],
                       foreground = colors[5],
                       padding = 0,
                       fontsize = 37
                       ),
              widget.TextBox(
                      text = " Vol:",
                       foreground = colors[2],
                       background = colors[5],
                       padding = 0
                       ),
              widget.Volume(
                       foreground = colors[2],
                       background = colors[5],
                       padding = 5
                       ),
              widget.TextBox(
                       text = '',
                       background = colors[5],
                       foreground = colors[4],
                       padding = 0,
                       fontsize = 37
                       ),
              widget.CurrentLayoutIcon(
                       custom_icon_paths = [os.path.expanduser("~/.config/qtile/icons")],
                       foreground = colors[0],
                       background = colors[4],
                       padding = 0,
                       scale = 0.7
                       ),
              widget.CurrentLayout(
                       foreground = colors[2],
                       background = colors[4],
                       padding = 5
                       ),
              widget.TextBox(
                       text = '',
                       background = colors[4],
                       foreground = colors[5],
                       padding = 0,
                       fontsize = 37
                       ),
              widget.Clock(
                       foreground = colors[2],
                       background = colors[5],
                       format = "%A, %B %d - %H:%M "
                       ),
              ]
    return widgets_list

def init_widgets_screen():
    widgets_screen2 = init_widgets_list()
    return widgets_screen2      

def init_screens():
    return [Screen(top=bar.Bar(widgets=init_widgets_screen(), opacity=1.0, size=20))]

if __name__ in ["config", "__main__"]:
    screens = init_screens()
    widgets_list = init_widgets_list()
    widgets_screen = init_widgets_screen()

def window_to_prev_group(qtile):
    if qtile.currentWindow is not None:
        i = qtile.groups.index(qtile.currentGroup)
        qtile.currentWindow.togroup(qtile.groups[i - 1].name)

def window_to_next_group(qtile):
    if qtile.currentWindow is not None:
        i = qtile.groups.index(qtile.currentGroup)
        qtile.currentWindow.togroup(qtile.groups[i + 1].name)

def window_to_previous_screen(qtile):
    i = qtile.screens.index(qtile.current_screen)
    if i != 0:
        group = qtile.screens[i - 1].group.name
        qtile.current_window.togroup(group)

def window_to_next_screen(qtile):
    i = qtile.screens.index(qtile.current_screen)
    if i + 1 != len(qtile.screens):
        group = qtile.screens[i + 1].group.name
        qtile.current_window.togroup(group)

def switch_screens(qtile):
    i = qtile.screens.index(qtile.current_screen)
    group = qtile.screens[i - 1].group
    qtile.current_screen.set_group(group)

mouse = [
    Drag([mod], "Button1", lazy.window.set_position_floating(),
         start=lazy.window.get_position()),
    Drag([mod], "Button3", lazy.window.set_size_floating(),
         start=lazy.window.get_size()),
    Click([mod], "Button2", lazy.window.bring_to_front())
]

dgroups_key_binder = None
dgroups_app_rules = []  # type: List
main = None
follow_mouse_focus = True
bring_front_click = False
cursor_warp = False

floating_layout = layout.Floating(float_rules=[
    # Run the utility of `xprop` to see the wm class and name of an X client.
    # default_float_rules include: utility, notification, toolbar, splash, dialog,
    # file_progress, confirm, download and error.
    *layout.Floating.default_float_rules,
    Match(title='Qalculate!'),        # qalculate-gtk
    Match(wm_class='kdenlive'),       # kdenlive
    Match(wm_class='pinentry-gtk-2'), # GPG key password entry
])
auto_fullscreen = True
focus_on_window_activation = "smart"

@hook.subscribe.startup_once
def start_once():
    home = os.path.expanduser('~')
    subprocess.call([home + '/.config/qtile/autostart.sh'])
wmname = "LG3D"
