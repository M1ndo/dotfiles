-- Configured added / removed by ybenel (github.com/m1ndo)
-- Modification Date: 10/24/2021
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
import XMonad.Actions.AfterDrag (afterDrag)
-- Controls
import XMonad.Actions.Navigation2D
  -- Data
import Data.Char (isSpace, toUpper)
import Data.Monoid
import Data.Maybe (isJust,fromJust)
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
import XMonad.Layout.Accordion
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
  xmproc0 <- spawnPipe "xmobar -x 0 $HOME/.config/xmobar/xmobarrc"
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
      , layoutHook         = myLayoutHook -- Add showWName' myShowWNameTheme $ If you want to print the current workspace in the screen
      , workspaces         = myWorkspaces
      , borderWidth        = myBorderWidth
      , normalBorderColor  = myNormColor
      , focusedBorderColor = myFocusColor
      , logHook = workspaceHistoryHook <+> myLogHook <+> fadeWindowsLogHook myFadeHook <+> dynamicLogWithPP xmobarPP
              { ppOutput = \x -> hPutStrLn xmproc0 x
              , ppCurrent = xmobarColor "#c792ea" "" . wrap "<box type=Bottom width=2 mb=2 color=#c792ea>" "</box>"
              , ppVisible = xmobarColor "#c792ea" "" . myClickableWorkspaces
              , ppHidden = xmobarColor "#82AAFF" "" . wrap "<box type=Top width=2 mt=2 color=#82AAFF>" "</box>" . myClickableWorkspaces
              , ppHiddenNoWindows = xmobarColor "#82AAFF" "" . myClickableWorkspaces
              , ppTitle = xmobarColor "#b3afc2" "" . shorten 60
              , ppSep =  "<fc=#666666> <fn=1>|</fn> </fc>"
              , ppUrgent = xmobarColor "#C45500" "" . wrap "!" "!"
              , ppExtras  = [windowCount]
              , ppOrder  = \(ws:l:t:ex) -> [ws,l]++ex++[t]
              }
              -- { ppOutput = \x -> hPutStrLn xmproc0 x
              -- , ppCurrent = xmobarColor "#98be65" "" . wrap "[" "]"
              -- , ppVisible = xmobarColor "#98be65" ""
              -- , ppHidden = xmobarColor "#82AAFF" "" . wrap "*" ""
              -- , ppHiddenNoWindows = xmobarColor "#c792ea" ""
              -- , ppTitle = xmobarColor "#b3afc2" "" . shorten 60
              -- , ppSep =  "<fc=#666666> <fn=2>|</fn> </fc>"
              -- , ppUrgent = xmobarColor "#C45500" "" . wrap "!" "!"
              -- , ppExtras  = [windowCount]
              -- , ppOrder  = \(ws:l:t:ex) -> [ws,l]++ex++[t]
              -- }
      } `additionalKeysP` myKeys

myFont :: String
myFont = "xft:Mononoki Nerd Font:bold:size=9:antialias=true:hinting=true"

myModMask :: KeyMask
myModMask = mod4Mask       -- Sets modkey to super/windows key

myTerminal :: String
myTerminal = "xterm"   -- Sets default terminal to the favorite (xterm)

myBrowser :: String
myBrowser = "firefox "               -- Moved To A better Browser .

myEditor :: String
myEditor = "emacsclient -c -a emacs"  -- Sets nvim as editor for tree select
-- myEditor = myTerminal ++ " -e vim "    -- Sets vim as editor for tree select

-- Setup A Emacs Client
myEmacs :: String
myEmacs = "emacsclient -c -a emacs"

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
        --spawn "/home/ybenel/.bin/ybl/resolution"
        spawnOnce "nitrogen --restore &"
        spawnOnce "picom &"
        spawnOnce "unclutter -root &"
        spawnOnce "sxhkd &"
        spawnOnce "nm-applet &"
        spawnOnce "volumeicon &"
        -- spawnOnce "trayer --edge top --align right --widthtype request --padding 6 --SetDockType true --SetPartialStrut true --expand true --monitor 0 --transparent true --alpha 10 --tint 0x282C34 --height 22 --iconspacing 0 --margin 682 &" -- Enable When Using Secondary Xmobar config
        spawnOnce "trayer --edge top --align right --widthtype request --padding 2 --SetDockType true --SetPartialStrut true --expand true --monitor 0 --transparent true --alpha 10 --tint 0x282C34 --height 22 --iconspacing 0 --margin 449 --distance 9 &"
        -- spawnOnce "trayer --edge top --align right --widthtype request --padding 6 --SetDockType true --SetPartialStrut true --expand true --monitor 0 --transparent true --alpha 10 --tint 0x090D12 --height 22 --iconspacing 0 --margin 476 &"
        spawnOnce "/usr/lib/polkit-kde-authentication-agent-1 &"
        spawnOnce "xfce4-power-manager &"
        spawnOnce "numlockx on &"
        -- spawnOnce "/usr/bin/emacs --daemon &"
        spawnOnce "xscreensaver -no-splash &"
        spawnOnce "caffeine &"
        -- spawn "/home/ybenel/.bin/ybl/jack_start"
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
               , ("Browser", "firefox")
               , ("Stremio", "stremio")
               , ("Lite", "lite")
               , ("Gimp", "gimp")
               , ("Discord", "discord")
               , ("Spotify", "spot_load")
               , ("LibreOffice Writer", "lowriter")
               , ("PCManFM", "pcmanfm")
               ]

-- Xmonad Shell Prompt Coordinates On The Screen (Default Centered)
ybCords :: XPPosition
ybCords = CenteredAt
     { xpCenterY = 0.5
     , xpWidth = 0.5
     }

ybXPConfig :: XPConfig
ybXPConfig = def
    { font                = myFont
    --, bgColor             = "#282c34"
    , fgColor             = "#bbc2cf"
    , bgHLight            = "#c792ea"
    , fgHLight            = "#000000"
    --, borderColor         = "#535974"
    , borderColor = "#090d12" -- Dark Xmobar
    , bgColor = "#090d12" -- Dark Xmobar
    , promptBorderWidth   = 0
    , promptKeymap        = ybXPKeymap
    , position            = ybCords
    , height              = 24
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
   , (xK_x, quit)                  -- quit out of prompt
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
browser  = S.selectSearchBrowser "/usr/bin/firefox"
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
              , NS "irssi" spawnIrc findIrc manageIrc
              , NS "mocp" spawnMocp findMocp manageMocp
              , NS "Ncp" spawnNcp findNcp manageNcp
              -- , NS "discord" spawnDiscord findDiscord manageDiscord
              -- , NS "lightcord" spawnLcord findLcord manageLcord
              , NS "qjackctl" spawnQjack findQjack manageQjack
              --, NS "spotify" spawnSpot findSpot manageSpot
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
  spawnMocp  = myTerminal ++ " -name mocp -e /usr/bin/mocp"
  findMocp   = appName =? "mocp"
  manageMocp = nonFloating

  spawnNcp  = myTerminal ++ " -name ncmpcpp -e ncmpcpp"
  findNcp   = appName =? "ncmpcpp"
  manageNcp = nonFloating

  spawnIrc  = myTerminal ++ " -n irssi -e 'torify irssi'"
  findIrc   = (stringProperty "WM_NAME" =? "irssi")
  manageIrc = customFloating $ W.RationalRect l t w h
            where
              h = 0.96
              w = 0.96
              t = 0.5
              l = 0.5
  -- spawnDiscord  = "discord"
  -- findDiscord   = (className =? "discord")
  -- manageDiscord = nonFloating

  -- spawnLcord  = "lightcord"
  -- findLcord   = (className =? "lightcord")
  -- manageLcord = nonFloating

  spawnQjack  = "qjackctl"
  findQjack   = (className =? "qjackctl")
  -- manageQjack = defaultFloating
  manageQjack = customFloating $ W.RationalRect l t w h
            where
              h = 0.96
              w = 0.96
              t = 0.5
              l = 0.5
  spawnSpot  = "spot_load"
  findSpot   = (className =? "spotify")
  manageSpot = nonFloating

mySpacing :: Integer -> l a -> XMonad.Layout.LayoutModifier.ModifiedLayout Spacing l a
mySpacing i = spacingRaw False (Border i i i i) True (Border i i i i) True

-- Below is a variation of the above except no borders are applied
-- if fewer than two windows. So a single window has no gaps.
mySpacing' :: Integer -> l a -> XMonad.Layout.LayoutModifier.ModifiedLayout Spacing l a
mySpacing' i = spacingRaw True (Border i i i i) True (Border i i i i) True

-- Defining a bunch of layouts, many that I don't use.
tall     = renamed [Replace "tall"]
           $ smartBorders
           $ addTabs shrinkText myTabTheme
           $ subLayout [] (smartBorders Simplest)
           $ limitWindows 12
           $ mySpacing 8
           $ ResizableTall 1 (3/100) (1/2) []
magnify  = renamed [Replace "magnify"]
           $ smartBorders
           $ addTabs shrinkText myTabTheme
           $ subLayout [] (smartBorders Simplest)
           $ magnifier
           $ limitWindows 12
           $ mySpacing 8
           $ ResizableTall 1 (3/100) (1/2) []
monocle  = renamed [Replace "monocle"]
           $ smartBorders
           $ addTabs shrinkText myTabTheme
           $ subLayout [] (smartBorders Simplest)
           $ limitWindows 20 Full
floats   = renamed [Replace "floats"]
           $ smartBorders
           $ limitWindows 20 simplestFloat
grid     = renamed [Replace "grid"]
           $ smartBorders
           $ addTabs shrinkText myTabTheme
           $ subLayout [] (smartBorders Simplest)
           $ limitWindows 12
           $ mySpacing 8
           $ mkToggle (single MIRROR)
           $ Grid (16/10)
spirals  = renamed [Replace "spirals"]
           $ smartBorders
           $ addTabs shrinkText myTabTheme
           $ subLayout [] (smartBorders Simplest)
           $ mySpacing' 8
           $ spiral (6/7)
threeCol = renamed [Replace "threeCol"]
           $ smartBorders
           $ addTabs shrinkText myTabTheme
           $ subLayout [] (smartBorders Simplest)
           $ limitWindows 7
           $ ThreeCol 1 (3/100) (1/2)
threeRow = renamed [Replace "threeRow"]
           $ smartBorders
           $ addTabs shrinkText myTabTheme
           $ subLayout [] (smartBorders Simplest)
           $ limitWindows 7
           $ Mirror
           $ ThreeCol 1 (3/100) (1/2)
tabs     = renamed [Replace "tabs"]
           $ tabbed shrinkText myTabTheme
tallAccordion  = renamed [Replace "tallAccordion"]
           $ Accordion
wideAccordion  = renamed [Replace "wideAccordion"]
           $ Mirror Accordion

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
            myDefaultLayout = tall ||| magnify ||| noBorders monocle ||| floats ||| noBorders tabs ||| grid ||| spirals ||| threeCol ||| threeRow ||| tallAccordion ||| wideAccordion

myWorkspaces = [" B ", " T ", " E ", " S ", " C ", " M "]

xmobarEscape :: String -> String
xmobarEscape = concatMap doubleLts
  where
      doubleLts '<' = "<<"
      doubleLts x   = [x]

myWorkspaceIndices = M.fromList $ zipWith (,) myWorkspaces [1..] -- (,) == \x y -> (x,y)

myClickableWorkspaces ws = "<action=xdotool key super+"++show i++">"++ws++"</action>"
    where i = fromJust $ M.lookup ws myWorkspaceIndices

-- myClickableWorkspaces :: [String]
-- myClickableWorkspaces = clickable . (map xmobarEscape)
--              $ [" B ", " T ", " E ", " S ", " C ", " M "]
--   where
--       clickable l = [ "<action=xdotool key super+" ++ show (n) ++ ">" ++ ws ++ "</action>" |
--                     (i,ws) <- zip [1..6] l,
--                     let n = i ]

myFadeHook = composeAll [className =? "lite" --> transparency 0.6
                        ,                opaque
                        ]
myManageHook :: XMonad.Query (Data.Monoid.Endo WindowSet)
myManageHook = composeAll
   -- using 'doShift ( myWorkspaces !! 3)' sends program to workspace 4!
   [ className =? "firefox"     --> doShift ( myWorkspaces !! 0 )
   , className =? "mpv"     --> doShift ( myWorkspaces !! 4 )
   , className =? "lite"     --> doShift ( myWorkspaces !! 2 )
   , className =? "vlc"     --> doShift ( myWorkspaces !! 3 )
   , className =? "stremio"     --> doShift ( myWorkspaces !! 3 )
   , className =? "spotify"     --> doShift ( myWorkspaces !! 5 )
   , className =? "Gimp"    --> doShift ( myWorkspaces !! 3 )
   , title =? "Oracle VM VirtualBox Manager"     --> doFloat
   , className =? "VirtualBox Manager" --> doShift  ( myWorkspaces !! 3 )
   , title =? "Oracle VM VirtualBox Manager"     --> doFloat
   , className =? "file_progress"   --> doFloat
   , className =? "dialog"          --> doFloat
   , className =? "download"        --> doFloat
   , className =? "error"           --> doFloat
   , className =? "notification"    --> doFloat
   , (className =? "firefox" <&&> resource =? "Dialog") --> doFloat  -- Float Firefox Dialog
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
      , ("M-b", spawn myBrowser)
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
     -- , ("C-t t", treeselectAction tsDefaultConfig)

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
      , ("M-S-<Space>", sendMessage ToggleStruts)     -- Toggles struts
      , ("M-<Space>", sendMessage (MT.Toggle NBFULL) >> sendMessage ToggleStruts) -- Toggles noborder/full
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

 -- AfterDrag Resize Window
     --, ((mod, button3), (\w -> focus w >> mouseResizeWindow w >> afterDrag (windows $ W.float w $ W.RationalRect 0 0 1 1)))
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
      , ("M-C-a", namedScratchpadAction myScratchPads "Ncp")
      -- , ("M-C-x", namedScratchpadAction myScratchPads "discord")
      -- , ("M-C-z", namedScratchpadAction myScratchPads "lightcord")
      , ("M-C-p", namedScratchpadAction myScratchPads "qjackctl")
      , ("M-C-y", namedScratchpadAction myScratchPads "spotify")

  -- Controls for mocp music player (SUPER-u followed by a key)
      , ("M-u p", spawn "mocp --play")
      , ("M-u l", spawn "mocp --next")
      , ("M-u h", spawn "mocp --previous")
      , ("M-u <Space>", spawn "mocp --toggle-pause")

  -- Controls for Ncmpcpp mpd music player (SUPER-u followed by a key)
      , ("M-u x", spawn "mpc toggle")
      , ("M-u v", spawn "mpc next")
      , ("M-u b", spawn "mpc prev")
      , ("M-u m", spawn "mpc stop")
  -- App Shortcuts
      , ("M1-<Return>", spawn "rofi -show drun -show-icons")
      , ("M-M1-s", spawn "dmenu_run -c -b -l 10 -g 4 -p 'ybenel: ' -fn 'scientifica:size=12'")
      , ("M-M1-e", spawn (myTerminal ++ " -e irssi"))
      , ("M-M1-c", spawn (myTerminal ++ " -e /usr/bin/mocp"))
      , ("M-e", spawn myEmacs)
      , ("M1-C-s", spawn "./.dmenu/dmenu-scrot.sh")
      , ("M1-C-b", spawn "./.dmenu/dmenu-setbg.sh")
      , ("M1-C-h", spawn "./.dmenu/dmenu-sysmon.sh")
      , ("M1-C-e", spawn "./.dmenu/dmenu-edit-configs.sh")
      , ("M-M1-z", spawn (myTerminal ++ " -e ncmpcpp"))

  -- Multimedia Keys
      , ("<XF86AudioPlay>", spawn (myTerminal ++ "mocp --play"))
      , ("<XF86AudioPrev>", spawn (myTerminal ++ "mocp --previous"))
      , ("<XF86AudioNext>", spawn (myTerminal ++ "mocp --next"))
      , ("<XF86AudioMute>",   spawn "amixer set Master toggle")
      , ("<XF86AudioLowerVolume>", spawn "amixer set Master 5%- unmute")
      , ("<XF86AudioRaiseVolume>", spawn "amixer set Master 5%+ unmute")
      , ("<XF86HomePage>", spawn "firefox")
      , ("<XF86Search>", safeSpawn "firefox" ["https://www.duckduckgo.com/"])
      , ("<XF86Mail>", runOrRaise "thunderbird" (resource =? "thunderbird"))
      , ("<XF86Calculator>", runOrRaise "gcalctool" (resource =? "gcalctool"))
      , ("<XF86Eject>", spawn "toggleeject")
      , ("<Print>", spawn "scrot")
      , ("M-<F1>", spawn "sxiv -r -q -t -o ~/wallpapers/*")
      , ("M-<F2>", spawn "/bin/ls ~/wallpapers | shuf -n 1 | xargs xwallpaper --stretch")
      ]
  -- Appending search engine prompts to keybindings list.
  -- Look at "search engines" section of this config for values for "k".
      ++ [("M-s " ++ k,S.promptSearchBrowser ybXPConfig' "/usr/bin/firefox" f) | (k,f) <- searchList ]
      ++ [("M-S-s " ++ k, S.selectSearch f) | (k,f) <- searchList ]
  -- Appending some extra xprompts to keybindings list.
  -- Look at "xprompt settings" section this of config for values for "k".
      ++ [("M-p " ++ k, f ybXPConfig') | (k,f) <- promptList ]
      ++ [("M-p " ++ k, f ybXPConfig' g) | (k,f,g) <- promptList' ]
  -- The following lines are needed for named scratchpads.
        where nonNSP          = WSIs (return (\ws -> W.tag ws /= "nsp"))
              nonEmptyNonNSP  = WSIs (return (\ws -> isJust (W.stack ws) && W.tag ws /= "nsp"))
              -- toggleCopyToAll = wsContainingCopies >>= \ws -> case ws of [] -> windows copyToAll _ -> killAllOtherCopies
