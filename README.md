# My Awesome Configuration
Awesome is a highly configurable, tiling window manager for X. It is very fast, extensible and licensed under the GNU GPLv2 license.  Awesome is a perfect "first" tiling window manager for those who want to try out tiling, but the extreme customization that you can achieve with awesome makes it a power user's dream.  Awesome is configured with Lua.

# Dotfiles Are Used For:
* AwesomeWM (Window Manager)
* Compton/Picom (Compositor)
* Rofi (Window Switcher)
* Dmenu (Dynamic Menu For X)
* Xterm (Terminal)
* st (Terminal)
* vifm (Terminal File Manager)
* neovim (Editor)
* mocp (Terminal Music Player)
* lynx (Minimal Terminal Browser)

# Features

* Simple enough for beginner's but flexible enough for the power user.
* Extremely customizable, maybe more so than any other window manager.
* Configured in Lua.
* A documented API to configure and define the behavior of your window manager.

# My Keybindings

The MODKEY is set to the Super key (aka the Windows key).  I try to keep the
keybindings consistent with all of my window managers.

| Keybinding | Action |
| :--- | :--- |
| `MODKEY + RETURN` | opens terminal (alacritty is the terminal but can be easily changed) |
| `MODKEY + SHIFT + RETURN` | opens run launcher (dmenu is the run launcher but can be easily changed) |
| `MODKEY + SHIFT + c` | closes window with focus |
| `MODKEY + SHIFT + r` | restarts awesome |
| `MODKEY + SHIFT + q` | quits awesome |
| `MODKEY + 1-9` | switch focus to workspace (1-9) |
| `MODKEY + SHIFT + 1-9` | send focused window to workspace (1-9) |
| `MODKEY + j` | switches focus between windows in the stack, going down |
| `MODKEY + k` | switches focus between windows in the stack, going up |
| `MODKEY + h` | switches focus between windows in the stack, going left |
| `MODKEY + l` | switches focus between windows in the stack, going right |
| `MODKEY + SHIFT + j` | rotates the windows in the stack, going down|
| `MODKEY + SHIFT + k` | rotates the windows in the stack, going up |
| `MODKEY + SHIFT + h` | rotates the windows in the stack, going left|
| `MODKEY + SHIFT + l` | rotates the windows in the stack, going right |
| `MODKEY + period` | switch focus to next monitor |
| `MODKEY + comma` | switch focus to prev monitor |

## Getting Help With Awesome
*Reporting issues*

Please report any issues you have with AwesomeWM on [our bugtracker](https://github.com/r2dr0dn/dotfiles/issues).

## Documentation For AwesomeWM

Online documentation is available [here](https://awesomewm.org/apidoc/).

## 

## License

The project is licensed under GNU General Public License v2 or later.
You can read it online at ([v2](http://www.gnu.org/licenses/gpl-2.0.html)
or [v3](http://www.gnu.org/licenses/gpl.html)).
