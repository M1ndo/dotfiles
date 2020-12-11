-- Reconfigured added / removed by ybenel (github.com/m1ndo)
-- Modification Date: 12/09/2020
-- Base
import XMonad
import System.IO (hPutStrLn)
import System.Exit (exitSuccess)
import qualified XMonad.StackSet as W

  -- Actions
import XMonad.Actions.CopyWindow (kill1, killAllOtherCopies,copyToAll,wsContainingCopies,killAllOtherCopies)
import XMonad.Actions.CycleWS (moveTo, shiftTo, WSType(..), nextScreen, prevScreen)
import XMonad.Actions.GridSelect
import XMonad.Actions.MouseResize
import XMonad.Actions.Promote
import XMonad.Actions.RotSlaves (rotSlavesDown, rotAllDown)
import qualified XMonad.Actions.TreeSelect as TS
import XMonad.Actions.WindowGo (runOrRaise)
import XMonad.Actions.WithAll (sinkAll, killAll)
import qualified XMonad.Actions.Search as S
import XMonad.Actions.Minimize
-- Controls
import XMonad.Actions.Navigation2D
  -- Data
import Data.Char (isSpace, toUpper)
import Data.Monoid
import Data.Maybe (isJust)
import Data.Tree
import qualified Data.Map as M

  -- Hooks
import XMonad.Hooks.DynamicLog (dynamicLogWithPP, wrap, xmobarPP, xmobarColor, shorten, PP(..))
import XMonad.Hooks.EwmhDesktops  -- for some fullscreen events, also for xcomposite in obs.
import XMonad.Hooks.FadeInactive
import XMonad.Hooks.ManageDocks (avoidStruts, docksEventHook, manageDocks, ToggleStruts(..))
import XMonad.Hooks.ManageHelpers (isFullscreen, doFullFloat)
import XMonad.Hooks.ServerMode
import XMonad.Hooks.SetWMName
import XMonad.Hooks.WorkspaceHistory
import XMonad.Hooks.FadeWindows
  -- Layouts
import XMonad.Layout.GridVariants (Grid(Grid))
import XMonad.Layout.SimplestFloat
import XMonad.Layout.Spiral
import XMonad.Layout.ResizableTile
import XMonad.Layout.Tabbed
import XMonad.Layout.ThreeColumns

  -- Layouts modifiers
import XMonad.Layout.LayoutModifier
import XMonad.Layout.LimitWindows (limitWindows, increaseLimit, decreaseLimit)
import XMonad.Layout.Magnifier
import XMonad.Layout.MultiToggle (mkToggle, single, EOT(EOT), (??))
import XMonad.Layout.MultiToggle.Instances (StdTransformers(NBFULL, MIRROR, NOBORDERS))
import XMonad.Layout.NoBorders
import XMonad.Layout.Renamed
import XMonad.Layout.ShowWName
import XMonad.Layout.Simplest
import XMonad.Layout.Spacing
import XMonad.Layout.SubLayouts
import XMonad.Layout.WindowNavigation
import XMonad.Layout.WindowArranger (windowArrange, WindowArrangerMsg(..))
import qualified XMonad.Layout.ToggleLayouts as T (toggleLayouts, ToggleLayout(Toggle))
import qualified XMonad.Layout.MultiToggle as MT (Toggle(..))
import XMonad.Layout.Minimize

  -- Prompt
import XMonad.Prompt
import XMonad.Prompt.Input
import XMonad.Prompt.FuzzyMatch
import XMonad.Prompt.Man
import XMonad.Prompt.Pass
import XMonad.Prompt.Shell
import XMonad.Prompt.Ssh
import XMonad.Prompt.XMonad
import Control.Arrow (first)

 -- Utilities
import XMonad.Util.EZConfig (additionalKeysP)
import XMonad.Util.NamedScratchpad
import XMonad.Util.Run (runProcessWithInput, safeSpawn, spawnPipe)
import XMonad.Util.SpawnOnce



main :: IO ()
main = do
  -- Launching One Instance Of xmobar.
  xmproc0 <- spawnPipe "xmobar -x 0 /home/ybenel/.config/xmobar/xmobarrc"
  -- the xmonad, ya know...what the WM is named after!
  xmonad $ navigation2D
          def
          (xK_Up, xK_Left, xK_Down, xK_Right)
          [(mod4Mask, windowGo), (mod4Mask .|. shiftMask, windowSwap)]
          False
      $ ewmh def
      { manageHook = ( isFullscreen --> doFullFloat ) <+> myManageHook <+> manageDocks
      , handleEventHook    = serverModeEventHookCmd
                             <+> serverModeEventHook
                             <+> serverModeEventHookF "XMONAD_PRINT" (io . putStrLn)
                             <+> docksEventHook
                             <+> fadeWindowsEventHook
      , modMask            = myModMask
      , terminal           = myTerminal
      , startupHook        = myStartupHook
      , layoutHook         = showWName' myShowWNameTheme $ myLayoutHook
      , workspaces         = myWorkspaces
      , borderWidth        = myBorderWidth
      , normalBorderColor  = myNormColor
      , focusedBorderColor = myFocusColor
      , logHook = workspaceHistoryHook <+> myLogHook <+> fadeWindowsLogHook myFadeHook <+> dynamicLogWithPP xmobarPP
                      { ppOutput = \x -> hPutStrLn xmproc0 x
                      , ppCurrent = xmobarColor "#98be65" "" . wrap "[" "]" -- Current workspace in xmobar
                      , ppVisible = xmobarColor "#98be65" ""                -- Visible but not current workspace
                      , ppHidden = xmobarColor "#82AAFF" "" . wrap "*" ""   -- Hidden workspaces in xmobar
                      , ppHiddenNoWindows = xmobarColor "#c792ea" ""        -- Hidden workspaces (no windows)
                      , ppTitle = xmobarColor "#b3afc2" "" . shorten 60     -- Title of active window in xmobar
                      , ppSep =  "<fc=#666666> <fn=2>|</fn> </fc>"          -- Separators in xmobar
                      , ppUrgent = xmobarColor "#C45500" "" . wrap "!" "!"  -- Urgent workspace
                      , ppExtras  = [windowCount]                           -- # of windows current workspace
                      , ppOrder  = \(ws:l:t:ex) -> [ws,l]++ex++[t]
                      }
      } `additionalKeysP` myKeys

myFont :: String
myFont = "xft:Mononoki Nerd Font:bold:size=9:antialias=true:hinting=true"

myModMask :: KeyMask
myModMask = mod4Mask       -- Sets modkey to super/windows key

myTerminal :: String
myTerminal = "xterm"   -- Sets default terminal to the favorite (xterm)

myBrowser :: String
myBrowser = "google-chrome-stable "               -- Sets chrome (crap Proprietary Garbage) as browser for tree select

myEditor :: String
myEditor = "nvim "  -- Sets nvim as editor for tree select
-- myEditor = myTerminal ++ " -e vim "    -- Sets vim as editor for tree select

myBorderWidth :: Dimension
myBorderWidth = 2          -- Sets border width for windows

myNormColor :: String
myNormColor   = "#282c34"  -- Border color of normal windows

myFocusColor :: String
myFocusColor  = "#46d9ff"  -- Border color of focused windows

altMask :: KeyMask
altMask = mod1Mask         -- Setting this for use in xprompts

windowCount :: X (Maybe String)
windowCount = gets $ Just . show . length . W.integrate' . W.stack . W.workspace . W.current . windowset

myStartupHook :: X ()
myStartupHook = do
        spawnOnce "nitrogen --restore &"
        spawnOnce "picom &"
        spawnOnce "nm-applet &"
        spawnOnce "volumeicon &"
        spawnOnce "trayer --edge top --align right --widthtype request --padding 6 --SetDockType true --SetPartialStrut true --expand true --monitor 0 --transparent true --alpha 10 --tint 0x1B2F31 --height 22 --iconspacing 0 --margin 682 &"
        spawnOnce "/usr/lib/polkit-kde-authentication-agent-1 &"
        spawnOnce "xfce4-power-manager"
        spawnOnce "numlockx on"
        spawn "/home/ybenel/.bin/ybl/jack_start"
        setWMName "LG3D"

myColorizer :: Window -> Bool -> X (String, String)
myColorizer = colorRangeFromClassName
                (0x28,0x2c,0x34) -- lowest inactive bg
                (0x28,0x2c,0x34) -- highest inactive bg
                (0xc7,0x92,0xea) -- active bg
                (0xc0,0xa7,0x9a) -- inactive fg
                (0x28,0x2c,0x34) -- active fg

-- gridSelect menu layout
mygridConfig :: p -> GSConfig Window
mygridConfig colorizer = (buildDefaultGSConfig myColorizer)
  { gs_cellheight   = 40
  , gs_cellwidth    = 200
  , gs_cellpadding  = 6
  , gs_originFractX = 0.5
  , gs_originFractY = 0.5
  , gs_font         = myFont
  }

spawnSelected' :: [(String, String)] -> X ()
spawnSelected' lst = gridselect conf lst >>= flip whenJust spawn
  where conf = def
                 { gs_cellheight   = 40
                 , gs_cellwidth    = 200
                 , gs_cellpadding  = 6
                 , gs_originFractX = 0.5
                 , gs_originFractY = 0.5
                 , gs_font         = myFont
                 }

myAppGrid = [ ("Xterm", "xterm")
               , ("Chrome", "google-chrome-stable")
               , ("Stremio", "stremio")
               , ("Atom", "atom")
               , ("Gimp", "gimp")
               , ("Discord", "Discord")
               , ("Spotify", "spot_load")
               , ("LibreOffice Impress", "loimpress")
               , ("LibreOffice Writer", "lowriter")
               , ("PCManFM", "pcmanfm")
               ]

treeselectAction :: TS.TSConfig (X ()) -> X ()
treeselectAction a = TS.treeselectAction a
 [ Node (TS.TSNode "+ Accessories" "Accessory applications" (return ()))
     [ Node (TS.TSNode "Archive Manager" "Tool for archived packages" (spawn "file-roller")) []
     , Node (TS.TSNode "Calculator" "Gui version of qalc" (spawn "qalculate-gtk")) []
     , Node (TS.TSNode "Nitrogen" "Wallpaper Drawer And Organizer" (spawn "nitrogen")) []
     , Node (TS.TSNode "RedShift" "Color Adjustment Tool" (spawn "redshift-gtk")) []
     , Node (TS.TSNode "Firejail" "Firejail Configurator" (spawn "firejail-ui")) []
     , Node (TS.TSNode "DarkOs Tweaks" "Tweak Ur Life" (spawn "darkos-tweaks")) []
     , Node (TS.TSNode "Virtualbox" "Oracle's virtualization program" (spawn "virtualbox")) []
     ]
 , Node (TS.TSNode "+ Graphics" "graphics programs" (return ()))
     [ Node (TS.TSNode "Gimp" "GNU image manipulation program" (spawn "gimp")) []
     , Node (TS.TSNode "Inkscape" "An SVG editing program" (spawn "inkscape")) []
     , Node (TS.TSNode "LibreOffice Draw" "LibreOffice drawing program" (spawn "lodraw")) []
     , Node (TS.TSNode "Shotcut" "Professional Video Editor" (spawn "shotcut")) []
     , Node (TS.TSNode "Krita" "Image Editor And Painter" (spawn "krita")) []
     ]
 , Node (TS.TSNode "+ Internet" "internet and web programs" (return ()))
     [ Node (TS.TSNode "Chrome" "Proprietary Shit Browser" (spawn "google-chrome-stable")) []
     , Node (TS.TSNode "Discord" "Chat and video chat platform" (spawn "Discord")) []
     , Node (TS.TSNode "Telegram" "Telegram Client" (spawn "telegram-desktop")) []
     , Node (TS.TSNode "Firefox" "Open source web browser" (spawn "firefox")) []
     , Node (TS.TSNode "IRssi" "Great IRC Client" (spawn (myTerminal ++ " -e irssi"))) []
     , Node (TS.TSNode "Qbittorrent" "Bittorrent client" (spawn "qbittorrent")) []
     ]
 , Node (TS.TSNode "+ Multimedia" "sound and video applications" (return ()))
     [ Node (TS.TSNode "Moc" "Ncurses music player" (spawn (myTerminal ++ " -e mocp"))) []
     , Node (TS.TSNode "Alsa Mixer" "Alsa volume control utility" (spawn (myTerminal ++ " -e alsamixer"))) []
     , Node (TS.TSNode "Clementine" "Advanced Music Player" (spawn "clementine")) []
     , Node (TS.TSNode "Audacity" "Graphical audio editing program" (spawn "audacity")) []
     , Node (TS.TSNode "Spotify" "Spotify Music" (spawn "spot_load")) []
     , Node (TS.TSNode "MPV" "multimedia player" (spawn "mpv")) []
     , Node (TS.TSNode "Stremio" "All In One For Everything" (spawn "stremio")) []
     , Node (TS.TSNode "VLC" "Multimedia player and server" (spawn "vlc")) []
     , Node (TS.TSNode "Ardour" "Audio Mix / Manager" (spawn "ardour")) []
     , Node (TS.TSNode "MusikCube" "Another Ncurses Music Player" (spawn (myTerminal ++ " -e musikcube"))) []
     ]
 , Node (TS.TSNode "+ Office" "office applications" (return ()))
     [ Node (TS.TSNode "LibreOffice" "Open source office suite" (spawn "libreoffice")) []
     , Node (TS.TSNode "LibreOffice Base" "Desktop database front end" (spawn "lobase")) []
     , Node (TS.TSNode "LibreOffice Calc" "Spreadsheet program" (spawn "localc")) []
     , Node (TS.TSNode "LibreOffice Draw" "Diagrams and sketches" (spawn "lodraw")) []
     , Node (TS.TSNode "LibreOffice Impress" "Presentation program" (spawn "loimpress")) []
     , Node (TS.TSNode "LibreOffice Math" "Formula editor" (spawn "lomath")) []
     , Node (TS.TSNode "LibreOffice Writer" "Word processor" (spawn "lowriter")) []
     , Node (TS.TSNode "Zathura" "PDF Viewer" (spawn "zathura")) []
     ]
 , Node (TS.TSNode "+ Programming" "programming and scripting tools" (return ()))
     [  Node (TS.TSNode "Nvim" "Boomers Text Editor" (spawn (myTerminal ++ " -e nvim"))) []
      , Node (TS.TSNode "Atom" "21 Century Editor" (spawn "atom")) []
      , Node (TS.TSNode "Sublime" "Sublime Text Editor" (spawn "subl3")) []
      , Node (TS.TSNode "Vscodium" "Vscode Without Garbage" (spawn "code")) []
      , Node (TS.TSNode "Meld" "Comparaison Program" (spawn "meld")) []
      , Node (TS.TSNode "Kate" "Kde Text Editor" (spawn "kate")) []
      , Node (TS.TSNode "Xed" "Simple Text Editor" (spawn "xed")) []
     ]
 , Node (TS.TSNode "+ System" "system tools and utilities" (return ()))
    [ Node (TS.TSNode "Xterm" "Old School Terminal" (spawn myTerminal)) []
    , Node (TS.TSNode "PCManFM" "Favorite file manager" (spawn "pcmanfm")) []
    , Node (TS.TSNode "Mint" "USB stick writer" (spawn (myTerminal ++ " -e mintstick -m iso"))) []
    , Node (TS.TSNode "Mint" "USB stick writer" (spawn (myTerminal ++ " -e mintstick -m format"))) []
    , Node (TS.TSNode "Glances" "Terminal system monitor" (spawn (myTerminal ++ " -e glances"))) []
    , Node (TS.TSNode "Gufw" "GUI uncomplicated firewall" (spawn "gufw")) []
    , Node (TS.TSNode "Htop" "Terminal process viewer" (spawn (myTerminal ++ " -e htop"))) []
    , Node (TS.TSNode "LXAppearance" "Customize look and feel" (spawn "lxappearance")) []
    , Node (TS.TSNode "Nmon" "Network monitor" (spawn (myTerminal ++ " -e nmon"))) []
    , Node (TS.TSNode "Simple Terminal" "Suckless simple terminal" (spawn "st")) []
    , Node (TS.TSNode "Vifm" "Vim-like file manager" (spawn (myTerminal ++ " -e vifm"))) []
    ]
 , Node (TS.TSNode "------------------------" "" (spawn "xdotool key Escape")) []
 , Node (TS.TSNode "+ Bookmarks" "a list of web bookmarks" (return ()))
     [ Node (TS.TSNode "+ Linux" "a list of web bookmarks" (return ()))
         [ Node (TS.TSNode "+ Arch Linux" "btw, i use arch!" (return ()))
             [ Node (TS.TSNode "Arch Linux" "Arch Linux homepage" (spawn (myBrowser ++ "https://www.archlinux.org/"))) []
             , Node (TS.TSNode "Arch Wiki" "The best Linux wiki" (spawn (myBrowser ++ "https://wiki.archlinux.org/"))) []
             , Node (TS.TSNode "AUR" "Arch User Repository" (spawn (myBrowser ++ "https://aur.archlinux.org/"))) []
             , Node (TS.TSNode "Arch Forums" "Arch Linux web forum" (spawn (myBrowser ++ "https://bbs.archlinux.org/"))) []
             ]
         , Node (TS.TSNode "+ Linux News" "linux news and blogs" (return ()))
             [ Node (TS.TSNode "DistroWatch" "Linux distro release announcments" (spawn (myBrowser ++ "https://distrowatch.com/"))) []
             , Node (TS.TSNode "LXer" "LXer linux news aggregation" (spawn (myBrowser ++ "http://lxer.com"))) []
             , Node (TS.TSNode "OMG Ubuntu" "Ubuntu news, apps and reviews" (spawn (myBrowser ++ "https://www.omgubuntu.co.uk"))) []
             ]
         , Node (TS.TSNode "+ Window Managers" "window manager documentation" (return ()))
             [ Node (TS.TSNode "+ Awesome" "awesomewm documentation" (return ()))
                 [ Node (TS.TSNode "Awesome" "Homepage for awesome wm" (spawn (myBrowser ++ "https://awesomewm.org/"))) []
                 , Node (TS.TSNode "Awesome GitHub" "The GutHub page for awesome" (spawn (myBrowser ++ "https://github.com/awesomeWM/awesome"))) []
                 , Node (TS.TSNode "r/awesome" "Subreddit for awesome" (spawn (myBrowser ++ "https://www.reddit.com/r/awesomewm/"))) []
                 ]
             , Node (TS.TSNode "+ Herbstluft" "herbstluft documentation" (return ()))
                 [ Node (TS.TSNode "Herbstluft" "Tiling window manager in Python" (spawn (myBrowser ++ "https://herbstluftwm.org/"))) []
                 , Node (TS.TSNode "Herbstluft GitHub" "The GitHub page for herbstluft" (spawn (myBrowser ++ "https://github.com/herbstluftwm/herbstluftwm"))) []
                 , Node (TS.TSNode "r/herbstluft" "Subreddit for herbstluft" (spawn (myBrowser ++ "https://www.reddit.com/r/herbstluftwm/"))) []
                 ]
             , Node (TS.TSNode "+ XMonad" "xmonad documentation" (return ()))
                 [ Node (TS.TSNode "XMonad" "Homepage for XMonad" (spawn (myBrowser ++ "http://xmonad.org"))) []
                 , Node (TS.TSNode "XMonad GitHub" "The GitHub page for XMonad" (spawn (myBrowser ++ "https://github.com/xmonad/xmonad"))) []
                 , Node (TS.TSNode "xmonad-contrib" "Third party extensions for XMonad" (spawn (myBrowser ++ "https://hackage.haskell.org/package/xmonad-contrib"))) []
                 , Node (TS.TSNode "xmonad-ontrib GitHub" "The GitHub page for xmonad-contrib" (spawn (myBrowser ++ "https://github.com/xmonad/xmonad-contrib"))) []
                 , Node (TS.TSNode "Xmobar" "Minimal text-based status bar"  (spawn (myBrowser ++ "https://hackage.haskell.org/package/xmobar"))) []
                 ]
             ]
         ]
     , Node (TS.TSNode "+ Search and Reference" "Search engines, indices and wikis" (return ()))
         [ Node (TS.TSNode "DuckDuckGo" "Privacy-oriented search engine" (spawn (myBrowser ++ "https://duckduckgo.com/"))) []
         , Node (TS.TSNode "Google" "The evil search engine" (spawn (myBrowser ++ "http://www.google.com"))) []
         , Node (TS.TSNode "Thesaurus" "Lookup synonyms and antonyms" (spawn (myBrowser ++ "https://www.thesaurus.com/"))) []
         , Node (TS.TSNode "Wikipedia" "The free encyclopedia" (spawn (myBrowser ++ "https://www.wikipedia.org/"))) []
         ]
     , Node (TS.TSNode "+ Programming" "programming and scripting" (return ()))
         [ Node (TS.TSNode "+ Bash and Shell Scripting" "shell scripting documentation" (return ()))
             [ Node (TS.TSNode "GNU Bash" "Documentation for bash" (spawn (myBrowser ++ "https://www.gnu.org/software/bash/manual/"))) []
             , Node (TS.TSNode "r/bash" "Subreddit for bash" (spawn (myBrowser ++ "https://www.reddit.com/r/bash/"))) []
             , Node (TS.TSNode "r/commandline" "Subreddit for the command line" (spawn (myBrowser ++ "https://www.reddit.com/r/commandline/"))) []
             , Node (TS.TSNode "Learn Shell" "Interactive shell tutorial" (spawn (myBrowser ++ "https://www.learnshell.org/"))) []
             ]
       , Node (TS.TSNode "+ Haskell" "haskell documentation" (return ()))
           [ Node (TS.TSNode "Haskell.org" "Homepage for haskell" (spawn (myBrowser ++ "http://www.haskell.org"))) []
           , Node (TS.TSNode "Hoogle" "Haskell API search engine" (spawn "https://hoogle.haskell.org/")) []
           , Node (TS.TSNode "r/haskell" "Subreddit for haskell" (spawn (myBrowser ++ "https://www.reddit.com/r/Python/"))) []
           , Node (TS.TSNode "Haskell on StackExchange" "Newest haskell topics on StackExchange" (spawn (myBrowser ++ "https://stackoverflow.com/questions/tagged/haskell"))) []
           ]
       , Node (TS.TSNode "+ Python" "python documentation" (return ()))
           [ Node (TS.TSNode "Python.org" "Homepage for python" (spawn (myBrowser ++ "https://www.python.org/"))) []
           , Node (TS.TSNode "r/Python" "Subreddit for python" (spawn (myBrowser ++ "https://www.reddit.com/r/Python/"))) []
           , Node (TS.TSNode "Python on StackExchange" "Newest python topics on StackExchange" (spawn (myBrowser ++ "https://stackoverflow.com/questions/tagged/python"))) []
           ]
       ]
     , Node (TS.TSNode "+ Vim" "vim and neovim documentation" (return ()))
         [ Node (TS.TSNode "Vim.org" "Vim, the ubiquitous text editor" (spawn (myBrowser ++ "https://www.vim.org/"))) []
         , Node (TS.TSNode "r/Vim" "Subreddit for vim" (spawn (myBrowser ++ "https://www.reddit.com/r/vim/"))) []
         , Node (TS.TSNode "Vi/m StackExchange" "Vi/m related questions" (spawn (myBrowser ++ "https://vi.stackexchange.com/"))) []
         ]
     , Node (TS.TSNode "My Start Page" "Custom start page for browser" (spawn (myBrowser ++ "file:///home/ybenel/Downloads/StartPage/homepage.html"))) []
     ]
 , Node (TS.TSNode "+ Config Files" "config files that edit often" (return ()))
     [ Node (TS.TSNode "xmobar" "status bar" (spawn (myEditor ++ "/home/ybenel/.config/xmobar/xmobarrc"))) []
     , Node (TS.TSNode "awesome" "awesome window manager" (spawn (myEditor ++ "/home/ybenel/.config/awesome/rc.lua"))) []
     , Node (TS.TSNode "bashrc" "the bourne again shell" (spawn (myEditor ++ "/home/ybenel/.bashrc"))) []
     , Node (TS.TSNode "dunst" "dunst notifications" (spawn (myEditor ++ "/home/ybenel/.config/dunst/dunstrc"))) []
     , Node (TS.TSNode "herbstluftwm" "herbstluft window manager" (spawn (myEditor ++ "/home/ybenel/.config/herbstluftwm/autostart"))) []
     , Node (TS.TSNode "neovim init.vim" "neovim text editor" (spawn (myEditor ++ "/home/ybenel/.Spacevim/config/init.vim"))) []
     , Node (TS.TSNode "polybar" "easy-to-use status bar" (spawn (myEditor ++ "/home/ybenel/.config/polybar/config.ini"))) []
     , Node (TS.TSNode "st config.h" "suckless simple terminal" (spawn (myEditor ++ "home/ybenel/Projects/Patches/st/config.h"))) []
     , Node (TS.TSNode "dmenu config.h" "surf web browser" (spawn (myEditor ++ "/home/ybenel/Projects/Patches/dmenu/config.h"))) []
     , Node (TS.TSNode "xresources" "xresources file" (spawn (myEditor ++ "/home/ybenel/.Xresources"))) []
     , Node (TS.TSNode "zshrc" "Config for the z shell" (spawn (myEditor ++ "/home/ybenel/.zshrc"))) []
     ]
 , Node (TS.TSNode "+ Screenshots" "take a screenshot" (return ()))
     [ Node (TS.TSNode "Quick fullscreen" "take screenshot immediately" (spawn "scrot -d 1 ~/Pictures/Screenshots/ybenel-full-%m-%d-@%H-%M-%S.png")) []
     , Node (TS.TSNode "Delayed fullscreen" "take screenshot in 5 secs" (spawn "scrot -d 5 ~/Pictures/Screenshots/ybenel-full-%m-%d-@%H-%M-%S.png")) []
     , Node (TS.TSNode "Section screenshot" "take screenshot of section" (spawn "scrot -a $(slop -f '%x,%y,%w,%h') -d 1 ~/Pictures/Screenshots/ybenel-area-%m-%d-@%H-%M-%S.png")) []
     ]
 , Node (TS.TSNode "------------------------" "" (spawn "xdotool key Escape")) []
 , Node (TS.TSNode "+ XMonad" "window manager commands" (return ()))
     [ Node (TS.TSNode "+ View Workspaces" "View a specific workspace" (return ()))
       [ Node (TS.TSNode "View 1" "View workspace 1" (spawn "~/.xmonad/xmonadctl 1")) []
       , Node (TS.TSNode "View 2" "View workspace 2" (spawn "~/.xmonad/xmonadctl 3")) []
       , Node (TS.TSNode "View 3" "View workspace 3" (spawn "~/.xmonad/xmonadctl 5")) []
       , Node (TS.TSNode "View 4" "View workspace 4" (spawn "~/.xmonad/xmonadctl 7")) []
       , Node (TS.TSNode "View 5" "View workspace 5" (spawn "~/.xmonad/xmonadctl 9")) []
       , Node (TS.TSNode "View 6" "View workspace 6" (spawn "~/.xmonad/xmonadctl 11")) []
       , Node (TS.TSNode "View 7" "View workspace 7" (spawn "~/.xmonad/xmonadctl 13")) []
       , Node (TS.TSNode "View 8" "View workspace 8" (spawn "~/.xmonad/xmonadctl 15")) []
       , Node (TS.TSNode "View 9" "View workspace 9" (spawn "~/.xmonad/xmonadctl 17")) []
       ]
     , Node (TS.TSNode "+ Shift Workspaces" "Send focused window to specific workspace" (return ()))
       [ Node (TS.TSNode "View 1" "View workspace 1" (spawn "~/.xmonad/xmonadctl 2")) []
       , Node (TS.TSNode "View 2" "View workspace 2" (spawn "~/.xmonad/xmonadctl 4")) []
       , Node (TS.TSNode "View 3" "View workspace 3" (spawn "~/.xmonad/xmonadctl 6")) []
       , Node (TS.TSNode "View 4" "View workspace 4" (spawn "~/.xmonad/xmonadctl 8")) []
       , Node (TS.TSNode "View 5" "View workspace 5" (spawn "~/.xmonad/xmonadctl 10")) []
       , Node (TS.TSNode "View 6" "View workspace 6" (spawn "~/.xmonad/xmonadctl 12")) []
       , Node (TS.TSNode "View 7" "View workspace 7" (spawn "~/.xmonad/xmonadctl 14")) []
       , Node (TS.TSNode "View 8" "View workspace 8" (spawn "~/.xmonad/xmonadctl 16")) []
       , Node (TS.TSNode "View 9" "View workspace 9" (spawn "~/.xmonad/xmonadctl 18")) []
       ]
     , Node (TS.TSNode "Next layout" "Switch to next layout" (spawn "~/.xmonad/xmonadctl next-layout")) []
     , Node (TS.TSNode "Recompile" "Recompile XMonad" (spawn "xmonad --recompile")) []
     , Node (TS.TSNode "Restart" "Restart XMonad" (spawn "xmonad --restart")) []
     , Node (TS.TSNode "Quit" "Restart XMonad" (io exitSuccess)) []
     ]
 , Node (TS.TSNode "+ System" "Shutdown/Suspend/Lock/Hibernate/Restart" (return ()))
     [ Node (TS.TSNode "Shutdown" "ShutDown The System" (spawn "systemctl poweroff")) []
     , Node (TS.TSNode "Restart" "Reboot The System" (spawn "systemctl reboot")) []
     , Node (TS.TSNode "Suspend" "Suspend The System" (spawn "systemctl suspend")) []
     , Node (TS.TSNode "Hibernate" "Hibernate The System" (spawn "systemctl hibernate")) []
     , Node (TS.TSNode "Lock" "Lock The Screen" (spawn "betterlockscreen -l -t 'This Is The Way'")) []
     ]
 ]

tsDefaultConfig :: TS.TSConfig a
tsDefaultConfig = TS.TSConfig { TS.ts_hidechildren = True
                            , TS.ts_background   = 0xdd282c34
                            , TS.ts_font         = myFont
                            , TS.ts_node         = (0xffd0d0d0, 0xff1c1f24)
                            , TS.ts_nodealt      = (0xffd0d0d0, 0xff282c34)
                            , TS.ts_highlight    = (0xffffffff, 0xff755999)
                            , TS.ts_extra        = 0xffd0d0d0
                            , TS.ts_node_width   = 200
                            , TS.ts_node_height  = 20
                            , TS.ts_originX      = 100
                            , TS.ts_originY      = 100
                            , TS.ts_indent       = 80
                            , TS.ts_navigate     = myTreeNavigation
                            }

myTreeNavigation = M.fromList
  [ ((0, xK_Escape),   TS.cancel)
  , ((0, xK_Return),   TS.select)
  , ((0, xK_space),    TS.select)
  , ((0, xK_Up),       TS.movePrev)
  , ((0, xK_Down),     TS.moveNext)
  , ((0, xK_Left),     TS.moveParent)
  , ((0, xK_Right),    TS.moveChild)
  , ((0, xK_k),        TS.movePrev)
  , ((0, xK_j),        TS.moveNext)
  , ((0, xK_h),        TS.moveParent)
  , ((0, xK_l),        TS.moveChild)
  , ((0, xK_o),        TS.moveHistBack)
  , ((0, xK_i),        TS.moveHistForward)
  , ((0, xK_a),        TS.moveTo ["+ Accessories"])
  , ((0, xK_g),        TS.moveTo ["+ Graphics"])
  , ((0, xK_i),        TS.moveTo ["+ Internet"])
  , ((0, xK_m),        TS.moveTo ["+ Multimedia"])
  , ((0, xK_o),        TS.moveTo ["+ Office"])
  , ((0, xK_p),        TS.moveTo ["+ Programming"])
  , ((0, xK_s),        TS.moveTo ["+ System"])
  , ((0, xK_b),        TS.moveTo ["+ Bookmarks"])
  , ((0, xK_c),        TS.moveTo ["+ Config Files"])
  , ((0, xK_r),        TS.moveTo ["+ Screenshots"])
  , ((mod4Mask, xK_l), TS.moveTo ["+ Bookmarks", "+ Linux"])
  , ((mod4Mask, xK_e), TS.moveTo ["+ Bookmarks", "+ Emacs"])
  , ((mod4Mask, xK_s), TS.moveTo ["+ Bookmarks", "+ Search and Reference"])
  , ((mod4Mask, xK_p), TS.moveTo ["+ Bookmarks", "+ Programming"])
  , ((mod4Mask, xK_v), TS.moveTo ["+ Bookmarks", "+ Vim"])
  , ((mod4Mask .|. altMask, xK_a), TS.moveTo ["+ Bookmarks", "+ Linux", "+ Arch Linux"])
  , ((mod4Mask .|. altMask, xK_n), TS.moveTo ["+ Bookmarks", "+ Linux", "+ Linux News"])
  , ((mod4Mask .|. altMask, xK_w), TS.moveTo ["+ Bookmarks", "+ Linux", "+ Window Managers"])
  ]

ybXPConfig :: XPConfig
ybXPConfig = def
    { font                = myFont
    , bgColor             = "#282c34"
    , fgColor             = "#bbc2cf"
    , bgHLight            = "#c792ea"
    , fgHLight            = "#000000"
    , borderColor         = "#535974"
    , promptBorderWidth   = 0
    , promptKeymap        = ybXPKeymap
    , position            = Top
    , height              = 20
    , historySize         = 256
    , historyFilter       = id
    , defaultText         = []
    , autoComplete        = Just 100000 -- set Just 100000 for .1 sec
    , showCompletionOnTab = False
    , searchPredicate     = fuzzyMatch
    , defaultPrompter     = id $ map toUpper  -- change prompt to UPPER
    , alwaysHighlight     = True
    , maxComplRows        = Nothing
    }

-- The same config above minus the autocomplete feature which is annoying
-- on certain Xprompts, like the search engine prompts.
ybXPConfig' :: XPConfig
ybXPConfig' = ybXPConfig
    { autoComplete        = Nothing
    }

-- A list of all of the standard Xmonad prompts and a key press assigned to them.
-- These are used in conjunction with keybinding I set later in the config.
promptList :: [(String, XPConfig -> X ())]
promptList = [ ("m", manPrompt)          -- manpages prompt
           , ("p", passPrompt)         -- get passwords (requires 'pass')
           , ("g", passGeneratePrompt) -- generate passwords (requires 'pass')
           , ("r", passRemovePrompt)   -- remove passwords (requires 'pass')
           , ("s", sshPrompt)          -- ssh prompt
           , ("x", xmonadPrompt)       -- xmonad prompt
           ]

-- Same as the above list except this is for my custom prompts.
promptList' :: [(String, XPConfig -> String -> X (), String)]
promptList' = [ ("c", calcPrompt, "qalc")         -- requires qalculate-gtk
            ]

calcPrompt c ans =
  inputPrompt c (trim ans) ?+ \input ->
      liftIO(runProcessWithInput "qalc" [input] "") >>= calcPrompt c
  where
      trim  = f . f
          where f = reverse . dropWhile isSpace

ybXPKeymap :: M.Map (KeyMask,KeySym) (XP ())
ybXPKeymap = M.fromList $
   map (first $ (,) controlMask)   -- control + <key>
   [ (xK_z, killBefore)            -- kill line backwards
   , (xK_k, killAfter)             -- kill line forwards
   , (xK_a, startOfLine)           -- move to the beginning of the line
   , (xK_e, endOfLine)             -- move to the end of the line
   , (xK_m, deleteString Next)     -- delete a character foward
   , (xK_b, moveCursor Prev)       -- move cursor forward
   , (xK_f, moveCursor Next)       -- move cursor backward
   , (xK_BackSpace, killWord Prev) -- kill the previous word
   , (xK_y, pasteString)           -- paste a string
   , (xK_g, quit)                  -- quit out of prompt
   , (xK_bracketleft, quit)
   ]
   ++
   map (first $ (,) altMask)       -- meta key + <key>
   [ (xK_BackSpace, killWord Prev) -- kill the prev word
   , (xK_f, moveWord Next)         -- move a word forward
   , (xK_b, moveWord Prev)         -- move a word backward
   , (xK_d, killWord Next)         -- kill the next word
   , (xK_n, moveHistory W.focusUp')   -- move up thru history
   , (xK_p, moveHistory W.focusDown') -- move down thru history
   ]
   ++
   map (first $ (,) 0) -- <key>
   [ (xK_Return, setSuccess True >> setDone True)
   , (xK_KP_Enter, setSuccess True >> setDone True)
   , (xK_BackSpace, deleteString Prev)
   , (xK_Delete, deleteString Next)
   , (xK_Left, moveCursor Prev)
   , (xK_Right, moveCursor Next)
   , (xK_Home, startOfLine)
   , (xK_End, endOfLine)
   , (xK_Down, moveHistory W.focusUp')
   , (xK_Up, moveHistory W.focusDown')
   , (xK_Escape, quit)
   ]

archwiki, ebay, news, reddit, urban :: S.SearchEngine
archwiki = S.searchEngine "archwiki" "https://wiki.archlinux.org/index.php?search="
ebay     = S.searchEngine "ebay" "https://www.ebay.com/sch/i.html?_nkw="
news     = S.searchEngine "news" "https://news.google.com/search?q="
reddit   = S.searchEngine "reddit" "https://www.reddit.com/search/?q="
urban    = S.searchEngine "urban" "https://www.urbandictionary.com/define.php?term="
browser  = S.selectSearchBrowser "/usr/local/bin/google-chrome-stable"
-- This is the list of search engines that I want to use. Some are from
-- XMonad.Actions.Search, and some are the ones that I added above.
searchList :: [(String, S.SearchEngine)]
searchList = [ ("a", archwiki)
           , ("d", S.duckduckgo)
           , ("e", ebay)
           , ("g", S.google)
           , ("h", S.hoogle)
           , ("i", S.images)
           , ("n", news)
           , ("r", reddit)
           , ("s", S.stackage)
           , ("t", S.thesaurus)
           , ("v", S.vocabulary)
           , ("b", S.wayback)
           , ("u", urban)
           , ("w", S.wikipedia)
           , ("y", S.youtube)
           , ("z", S.amazon)
           ]

myScratchPads :: [NamedScratchpad]
myScratchPads = [ NS "terminal" spawnTerm findTerm manageTerm
              , NS "mocp" spawnMocp findMocp manageMocp
              , NS "irssi" spawnIrc findIrc manageIrc
              , NS "discord" spawnDiscord findDiscord manageDiscord
              , NS "qjackctl" spawnQjack findQjack manageQjack
              ]
 where
  spawnTerm  = myTerminal ++ " -n scratchpad"
  findTerm   = resource =? "scratchpad"
  manageTerm = customFloating $ W.RationalRect l t w h
             where
               h = 0.9
               w = 0.9
               t = 0.95 -h
               l = 0.95 -w
  spawnMocp  = myTerminal ++ " -name mocp -e mocp"
  findMocp   = appName =? "mocp"
  manageMocp = nonFloating

  spawnIrc  = myTerminal ++ " -n irssi -e 'irssi'"
  findIrc   = (stringProperty "WM_NAME" =? "irssi")
  manageIrc = customFloating $ W.RationalRect l t w h
            where
              h = 0.96
              w = 0.96
              t = 0.5
              l = 0.5
  spawnDiscord  = "Discord"
  findDiscord   = (className =? "discord")
  manageDiscord = nonFloating

  spawnQjack  = "qjackctl"
  findQjack   = (className =? "qjackctl")
  manageQjack = defaultFloating

mySpacing :: Integer -> l a -> XMonad.Layout.LayoutModifier.ModifiedLayout Spacing l a
mySpacing i = spacingRaw False (Border i i i i) True (Border i i i i) True

-- Below is a variation of the above except no borders are applied
-- if fewer than two windows. So a single window has no gaps.
mySpacing' :: Integer -> l a -> XMonad.Layout.LayoutModifier.ModifiedLayout Spacing l a
mySpacing' i = spacingRaw True (Border i i i i) True (Border i i i i) True

-- Defining a bunch of layouts, many that I don't use.
tall     = renamed [Replace "tall"]
         $ windowNavigation
         $ addTabs shrinkText myTabTheme
         $ subLayout [] (smartBorders Simplest)
         $ limitWindows 12
         $ mySpacing 8
         $ ResizableTall 1 (3/100) (1/2) []
magnify  = renamed [Replace "magnify"]
         $ windowNavigation
         $ addTabs shrinkText myTabTheme
         $ subLayout [] (smartBorders Simplest)
         $ magnifier
         $ limitWindows 12
         $ mySpacing 8
         $ ResizableTall 1 (3/100) (1/2) []
monocle  = renamed [Replace "monocle"]
         $ windowNavigation
         $ addTabs shrinkText myTabTheme
         $ subLayout [] (smartBorders Simplest)
         $ limitWindows 20 Full
floats   = renamed [Replace "floats"]
         $ windowNavigation
         $ addTabs shrinkText myTabTheme
         $ subLayout [] (smartBorders Simplest)
         $ limitWindows 20 simplestFloat
grid     = renamed [Replace "grid"]
         $ windowNavigation
         $ addTabs shrinkText myTabTheme
         $ subLayout [] (smartBorders Simplest)
         $ limitWindows 12
         $ mySpacing 8
         $ mkToggle (single MIRROR)
         $ Grid (16/10)
spirals  = renamed [Replace "spirals"]
         $ windowNavigation
         $ addTabs shrinkText myTabTheme
         $ subLayout [] (smartBorders Simplest)
         $ mySpacing' 8
         $ spiral (6/7)
threeCol = renamed [Replace "threeCol"]
         $ windowNavigation
         $ addTabs shrinkText myTabTheme
         $ subLayout [] (smartBorders Simplest)
         $ limitWindows 7
         $ mySpacing' 4
         $ ThreeCol 1 (3/100) (1/2)
threeRow = renamed [Replace "threeRow"]
         $ windowNavigation
         $ addTabs shrinkText myTabTheme
         $ subLayout [] (smartBorders Simplest)
         $ limitWindows 7
         $ mySpacing' 4
         -- Mirror takes a layout and rotates it by 90 degrees.
         -- So we are applying Mirror to the ThreeCol layout.
         $ Mirror
         $ ThreeCol 1 (3/100) (1/2)
tabs     = renamed [Replace "tabs"]
         -- I cannot add spacing to this layout because it will
         -- add spacing between window and tabs which looks bad.
         $ tabbed shrinkText myTabTheme

myTabTheme = def { fontName            = myFont
               , activeColor         = "#46d9ff"
               , inactiveColor       = "#313846"
               , activeBorderColor   = "#46d9ff"
               , inactiveBorderColor = "#282c34"
               , activeTextColor     = "#282c34"
               , inactiveTextColor   = "#d0d0d0"
               }

-- Theme for showWName which prints current workspace when you change workspaces.
myShowWNameTheme :: SWNConfig
myShowWNameTheme = def
  { swn_font              = "xft:Ubuntu:bold:size=60"
  , swn_fade              = 1.0
  , swn_bgcolor           = "#1c1f24"
  , swn_color             = "#ffffff"
  }

-- The layout hook
myLayoutHook = avoidStruts . minimize $ mouseResize $ windowArrange $ T.toggleLayouts floats
             $ mkToggle (NBFULL ?? NOBORDERS ?? EOT) myDefaultLayout
           where
            myDefaultLayout = tall ||| magnify ||| noBorders monocle ||| floats ||| noBorders tabs ||| grid ||| spirals ||| threeCol ||| threeRow

myWorkspaces = [" B ", " T ", " E ", " G ", " M ", " C "]

xmobarEscape :: String -> String
xmobarEscape = concatMap doubleLts
  where
      doubleLts '<' = "<<"
      doubleLts x   = [x]

myClickableWorkspaces :: [String]
myClickableWorkspaces = clickable . (map xmobarEscape)
             $ [" B ", " T ", " E ", " G ", " M ", " C "]
  where
      clickable l = [ "<action=xdotool key super+" ++ show (n) ++ ">" ++ ws ++ "</action>" |
                    (i,ws) <- zip [1..6] l,
                    let n = i ]

myFadeHook = composeAll [className =? "atom" --> transparency 0.6
                        ,                opaque
                        ]
myManageHook :: XMonad.Query (Data.Monoid.Endo WindowSet)
myManageHook = composeAll
   -- using 'doShift ( myWorkspaces !! 3)' sends program to workspace 4!
   [ title =? "Google Chrome"     --> doShift ( myWorkspaces !! 0 )
   , className =? "mpv"     --> doShift ( myWorkspaces !! 4 )
   , className =? "atom"     --> doShift ( myWorkspaces !! 2 )
   , className =? "vlc"     --> doShift ( myWorkspaces !! 3 )
   , className =? "stremio"     --> doShift ( myWorkspaces !! 3 )
   , className =? "Gimp"    --> doShift ( myWorkspaces !! 3 )
   , className =? "Gimp"    --> doFloat
   , title =? "Oracle VM VirtualBox Manager"     --> doFloat
   , className =? "VirtualBox Manager" --> doShift  ( myWorkspaces !! 3 )
   , (className =? "google-chrome" <&&> resource =? "Dialog") --> doFloat  -- Float Firefox Dialog
   ] <+> namedScratchpadManageHook myScratchPads

myLogHook :: X ()
myLogHook = fadeInactiveLogHook fadeAmount
  where fadeAmount = 1.0

myKeys :: [(String, X ())]
myKeys =
  -- Xmonad
      [ ("M-C-r", spawn "xmonad --recompile") -- Recompiles xmonad
      , ("M-S-r", spawn "xmonad --restart")   -- Restarts xmonad
      , ("M-S-q", io exitSuccess)             -- Quits xmonad

  -- Run Prompt
      , ("M-S-<Return>", shellPrompt ybXPConfig) -- Shell Prompt

  -- Useful programs to have a keybinding for launch
      , ("M-<Return>", spawn myTerminal)
      , ("M-b", spawn (myBrowser ++ " file:///home/ybenel/Downloads/StartPage/homepage.html"))
      , ("M-M1-h", spawn (myTerminal ++ " -e htop"))

  -- Kill windows
      , ("M-S-c", kill1)                         -- Kill the currently focused client
      , ("M-S-a", killAll)                       -- Kill all windows on current workspace

  -- Workspaces
      , ("M-.", nextScreen)  -- Switch focus to next monitor
      , ("M-,", prevScreen)  -- Switch focus to prev monitor
      , ("M-S-<KP_Add>", shiftTo Next nonNSP >> moveTo Next nonNSP)       -- Shifts focused window to next ws
      , ("M-S-<KP_Subtract>", shiftTo Prev nonNSP >> moveTo Prev nonNSP)  -- Shifts focused window to prev ws

  -- Floating windows
      , ("M-f", sendMessage (T.Toggle "floats")) -- Toggles my 'floats' layout
      , ("M-t", withFocused $ windows . W.sink)  -- Push floating window back to tile
      , ("M-S-t", sinkAll)                       -- Push ALL floating windows to tile

  -- Increase/decrease spacing (gaps)
      , ("M-d", decWindowSpacing 4)           -- Decrease window spacing
      , ("M-i", incWindowSpacing 4)           -- Increase window spacing
      , ("M-S-d", decScreenSpacing 4)         -- Decrease screen spacing
      , ("M-S-i", incScreenSpacing 4)         -- Increase screen spacing

  -- Grid Select (CTR-g followed by a key)
      , ("C-g g", spawnSelected' myAppGrid)                 -- grid select favorite apps
      , ("C-g t", goToSelected $ mygridConfig myColorizer)  -- goto selected window
      , ("C-g b", bringSelected $ mygridConfig myColorizer) -- bring selected window

  -- Tree Select
      , ("C-t t", treeselectAction tsDefaultConfig)

  -- Windows navigation
      , ("M-m", windows W.focusMaster)  -- Move focus to the master window
      , ("M-j", windows W.focusDown)    -- Move focus to the next window
      , ("M-k", windows W.focusUp)      -- Move focus to the prev window
      , ("M-<Up>", windows W.focusUp)    -- Move focus to the next window
      , ("M-<Down>", windows W.focusDown)    -- Move focus to the next window
      , ("M-S-m", windows W.swapMaster) -- Swap the focused window and the master window
      , ("M-S-j", windows W.swapDown)   -- Swap focused window with next window
      , ("M-S-k", windows W.swapUp)     -- Swap focused window with prev window
      , ("M-S-<Up>", windows W.swapUp)
      , ("M-S-<Down>", windows W.swapDown)
      , ("M-<Backspace>", promote)      -- Moves focused window to master, others maintain order
      , ("M-S-<Tab>", rotSlavesDown)    -- Rotate all windows except master and keep focus in place
      , ("M-C-<Tab>", rotAllDown)       -- Rotate all the windows in the current stack

  -- Layouts
      , ("M-<Tab>", sendMessage NextLayout)           -- Switch to next layout
      , ("M-C-M1-<Up>", sendMessage Arrange)
      , ("M-C-M1-<Down>", sendMessage DeArrange)
      , ("M-<Space>", sendMessage (MT.Toggle NBFULL) >> sendMessage ToggleStruts) -- Toggles noborder/full
      , ("M-S-<Space>", sendMessage ToggleStruts)     -- Toggles struts
      , ("M-S-n", sendMessage $ MT.Toggle NOBORDERS)  -- Toggles noborder
      , ("M-C-d", windows $ copyToAll)

  -- Increase/decrease windows in the master pane or the stack
      , ("M-S-<Up>", sendMessage (IncMasterN 1))      -- Increase number of clients in master pane
      , ("M-S-<Down>", sendMessage (IncMasterN (-1))) -- Decrease number of clients in master pane
      , ("M-C-<Up>", increaseLimit)                   -- Increase number of windows
      , ("M-C-<Down>", decreaseLimit)                 -- Decrease number of windows

  -- Window resizing
      , ("M-h", sendMessage Shrink)                   -- Shrink horiz window width
      , ("M-l", sendMessage Expand)                   -- Expand horiz window width
      , ("M-M1-j", sendMessage MirrorShrink)          -- Shrink vert window width
      , ("M-M1-k", sendMessage MirrorExpand)          -- Exoand vert window width

 -- Window Minimize / Maximize
      , ("M-n", withFocused minimizeWindow)
      , ("M-C-n", withLastMinimized maximizeWindow)
  -- Sublayouts
  -- This is used to push windows to tabbed sublayouts, or pull them out of it.
      , ("M-C-h", sendMessage $ pullGroup L)
      , ("M-C-l", sendMessage $ pullGroup R)
      , ("M-C-k", sendMessage $ pullGroup U)
      , ("M-C-j", sendMessage $ pullGroup D)
      , ("M-C-m", withFocused (sendMessage . MergeAll))
      , ("M-C-u", withFocused (sendMessage . UnMerge))
      , ("M-C-/", withFocused (sendMessage . UnMergeAll))
      , ("M-C-.", onGroup W.focusUp')    -- Switch focus to next tab
      , ("M-C-,", onGroup W.focusDown')  -- Switch focus to prev tab

  -- Scratchpads
      , ("M-C-<Return>", namedScratchpadAction myScratchPads "terminal")
      , ("M-C-c", namedScratchpadAction myScratchPads "mocp")
      , ("M-C-e", namedScratchpadAction myScratchPads "irssi")
      , ("M-C-x", namedScratchpadAction myScratchPads "discord")
      , ("M-C-p", namedScratchpadAction myScratchPads "qjackctl")

  -- Controls for mocp music player (SUPER-u followed by a key)
      , ("M-u p", spawn "mocp --play")
      , ("M-u l", spawn "mocp --next")
      , ("M-u h", spawn "mocp --previous")
      , ("M-u <Space>", spawn "mocp --toggle-pause")

  -- App Shortcuts
      , ("M-C-s", spawn "rofi -combi-modi run,drun -show combi -modi combi -show-icons -icon-theme 'Breeze' -display-combi 'ybenel: '")
      , ("M-M1-s", spawn "dmenu_run -c -bw 2 -l 10 -g 4 -p 'ybenel: ' -fn 'scientifica:size=12'")
      , ("M-M1-e", spawn (myTerminal ++ " -e irssi"))
      , ("M-M1-c", spawn (myTerminal ++ " -e mocp"))
      , ("M-e", spawn (myTerminal ++ " -e nvim"))
      , ("M1-C-s", spawn "./.dmenu/dmenu-scrot.sh")
      , ("M1-C-h", spawn "./.dmenu/dmenu-sysmon.sh")
      , ("M1-C-e", spawn "./.dmenu/dmenu-edit-configs.sh")

  -- Multimedia Keys
      , ("<XF86AudioPlay>", spawn (myTerminal ++ "mocp --play"))
      , ("<XF86AudioPrev>", spawn (myTerminal ++ "mocp --previous"))
      , ("<XF86AudioNext>", spawn (myTerminal ++ "mocp --next"))
      , ("<XF86AudioMute>",   spawn "amixer set Master toggle")
      , ("<XF86AudioLowerVolume>", spawn "amixer set Master 5%- unmute")
      , ("<XF86AudioRaiseVolume>", spawn "amixer set Master 5%+ unmute")
      , ("<XF86HomePage>", spawn "firefox")
      , ("<XF86Search>", safeSpawn "google-chrome-stable" ["https://www.duckduckgo.com/"])
      , ("<XF86Mail>", runOrRaise "thunderbird" (resource =? "thunderbird"))
      , ("<XF86Calculator>", runOrRaise "gcalctool" (resource =? "gcalctool"))
      , ("<XF86Eject>", spawn "toggleeject")
      , ("<Print>", spawn "scrot")
      ]
  -- Appending search engine prompts to keybindings list.
  -- Look at "search engines" section of this config for values for "k".
      ++ [("M-s " ++ k,S.promptSearchBrowser ybXPConfig' "/usr/local/bin/google-chrome-stable" f) | (k,f) <- searchList ]
      ++ [("M-S-s " ++ k, S.selectSearch f) | (k,f) <- searchList ]
  -- Appending some extra xprompts to keybindings list.
  -- Look at "xprompt settings" section this of config for values for "k".
      ++ [("M-p " ++ k, f ybXPConfig') | (k,f) <- promptList ]
      ++ [("M-p " ++ k, f ybXPConfig' g) | (k,f,g) <- promptList' ]
  -- The following lines are needed for named scratchpads.
        where nonNSP          = WSIs (return (\ws -> W.tag ws /= "nsp"))
              nonEmptyNonNSP  = WSIs (return (\ws -> isJust (W.stack ws) && W.tag ws /= "nsp"))
              -- toggleCopyToAll = wsContainingCopies >>= \ws -> case ws of [] -> windows copyToAll _ -> killAllOtherCopies
